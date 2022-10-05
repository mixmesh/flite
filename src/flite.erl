%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(flite).

-on_load(init/0).
-export([list_voices/0]).
-export([text_to_wave/1]).
-export([text_to_wave/2]).
-export([text_to_wave/3]).
%% util
-export([list_lib_voices/0]).
-export([say/1, say/2, say/3, say/4]).
-export([aplay/1, aplay/2]).
-export([pitch/2]).
-export([pause/3]).
-export([word_split/2]).

-type unsigned() :: non_neg_integer().
-type wave_header_entry() ::
	{format, alsa:format()} |
	{rate, unsigned()} |
	{channels, unsigned()}.

-type wave_header() :: [wave_header_entry()].

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(flite), "flite_nif"),
    Voices = list_lib_voices(),
    erlang:load_nif(Nif, Voices).

voices_dir()->
    Arch = string:trim(os:cmd("gcc -dumpmachine")),
    filename:join("/usr/lib/", Arch).

-spec list_voices() -> [{Lang::string(),Name::string()}].
list_voices() ->
    ?nif_stub().

list_lib_voices() ->
    filelib:wildcard(filename:join(voices_dir(),"libflite_cmu_*.so")).

-spec text_to_wave(Text::string()) ->
	  {Header::wave_header(), Samples::binary()}.
text_to_wave(Text) ->
    text_to_wave(Text, "us", "slt").

-spec text_to_wave(Text::string(), Voice::string()) ->
	  {Header::wave_header(), Samples::binary()}.
text_to_wave(Text, Voice) ->
    text_to_wave(Text, "us", Voice).

-spec text_to_wave(Text::string(), Lang::string(), Voice::string()) ->
	  {Header::wave_header(), Samples::binary()}.
text_to_wave(_Text, _Lang, _Voice) ->
    ?nif_stub().

say(Text) ->
    aplay(text_to_wave(Text), []).

say(Text,Params) when is_list(Params) ->
    aplay(text_to_wave(Text), Params).

say(Text, Lang, Voice) ->
    aplay(text_to_wave(Text, Lang, Voice), []).

say(Text, Lang, Voice, Params) when is_list(Params) ->
    aplay(text_to_wave(Text, Lang, Voice), Params).

pitch(Factor, {Header, Src}) when is_number(Factor), Factor > 0 ->
    SrcRate = proplists:get_value(rate, Header, 16000),
    DstRate = round(SrcRate/Factor),
    Format = proplists:get_value(format, Header, s16_le),
    Channels = proplists:get_value(channels, Header, 1),
    Dst = alsa_samples:resample(SrcRate, DstRate, Format, Channels, Src),
    %% do NOT change rate...
    {Header, Dst}.

%% locate pauses and cut it into piece
word_split(T, {Header,Src}) when is_number(T) ->
    SrcRate = proplists:get_value(rate, Header, 16000),
    Format = proplists:get_value(format, Header, s16_le),
    Channels = proplists:get_value(channels, Header, 1),
    FrameSize = alsa:format_size(Format, Channels),
    Separator = silence,
    Inject =
	case Separator of
	    tone ->
		NoteTime = 0.1,
		W = alsa_samples:create_wave(
		      SrcRate,
		      [{envelope,0,[NoteTime]},
		       {wave,0,[#{form=>sine,freq=>"A5",level=>0.2},
				#{form=>sine,freq=>"A5",level=>0.2}
			       ]}]),
		NoteLen = round(NoteTime*SrcRate),
		alsa_samples:wave(W, Format, Channels, NoteLen);
	    silence ->
		alsa:make_silence(Format, Channels, round(1.0*SrcRate))
	end,
    Src1 = alsa_samples:filter(Format, Format, [1.0,-0.937], Src),
    Len = round(T*SrcRate),  %% chunk Time as number of frames
    Chunks = alsa_util:chunks_of_len(Len, Format, Channels, Src1),
    Power = alsa_util:power_norm(Chunks, Format, Channels),
    ChunkLen = Len*FrameSize,  %% number of bytes per chunk!
    Words = split0_(Src, Power, ChunkLen, Inject, []),
    {Header, list_to_binary(Words)}.

split0_(Src, [P|Ps], ChunkLen, Inject, Acc) when byte_size(Src) >= ChunkLen ->
    if P =< 0.1 ->
	    <<_:ChunkLen/binary,Src1/binary>> = Src,
	    split0_(Src1, Ps, ChunkLen, Inject, Acc);
       true ->
	    <<Chunk:ChunkLen/binary,Src1/binary>> = Src,
	    split1_(Src1, Ps, ChunkLen, Inject, [Chunk|Acc])
    end;
split0_(_Src, _Ps, _ChunkLen, _Inkect, Acc) ->
    lists:reverse(Acc).

split1_(Src, [P|Ps], ChunkLen, Inject, Acc) when byte_size(Src) >= ChunkLen ->
    if P =< 0.1 ->
	    <<_:ChunkLen/binary,Src1/binary>> = Src,
	    split0_(Src1, Ps, ChunkLen, Inject, [Inject|Acc]);
       true ->
	    <<Chunk:ChunkLen/binary,Src1/binary>> = Src,
	    split1_(Src1, Ps, ChunkLen, Inject, [Chunk|Acc])
    end;
split1_(_Src, _Ps, _ChunkLen, _Inject, Acc) ->
    lists:reverse(Acc).

%% insert N pauses of duration Duration into Src
pause(N, Duration, {Header,Src}) ->
    Rate = proplists:get_value(rate, Header, 16000),
    Format = proplists:get_value(format, Header, s16_le),
    Channels = proplists:get_value(channels, Header, 1),
    NSilence = round(Duration * Rate),
    Silence = alsa:make_silence(Format, Channels, NSilence),
    %% split Src in N-1 "equally" sized parts
    FrameSize = alsa:format_size(Format, Channels),
    NumFrames = byte_size(Src) div FrameSize,
    DstFrames = NumFrames div (N+1),
    DstBytes  = DstFrames*FrameSize,
    {Header,list_to_binary(split_pause_(Src, DstBytes, Silence))}.

split_pause_(Bin, Len, Silence) when byte_size(Bin) > Len ->
    <<Chunk:Len/binary, Bin1/binary>> = Bin,
    [Chunk, Silence | split_pause_(Bin1, Len, Silence)];
split_pause_(Bin, _Len, _Silence) when byte_size(Bin) > 0 ->
    [Bin];
split_pause_(_, _, _) ->
    [].
    
aplay(Wave) ->
    aplay(Wave, []).
aplay(Wave={Params, Samples}, Params0) when is_list(Params0) ->
    case file:open(Samples, [ram, read, binary]) of
	{ok,Fd} ->
	    Params1 = Params ++ Params0,
	    try alsa_playback:fd(Fd, Params1) of
		_Result -> Wave
	    after
		file:close(Fd)
	    end;
	{error,Reason} ->
	    {error, file:format_error(Reason)}
    end.
	   
