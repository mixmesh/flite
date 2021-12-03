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
-type wave_header() ::
	{WaveType::atom(), SampleRate::number(),
	 Channels::integer(), ChannelMap::integer()}.

-define(VOICES_DIR, "/usr/lib/x86_64-linux-gnu").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(flite), "flite_nif"),
    Voices = list_lib_voices(),
    erlang:load_nif(Nif, Voices).

-spec list_voices() -> [{Lang::string(),Name::string()}].
list_voices() ->
    ?nif_stub().

list_lib_voices() ->
    filelib:wildcard(filename:join(?VOICES_DIR,"libflite_cmu_*.so")).


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
    aplay(text_to_wave(Text), #{}).
say(Text,Params) when is_map(Params) ->
    aplay(text_to_wave(Text), Params);
say(Text,Voice) ->
    aplay(text_to_wave(Text,Voice), #{}).
say(Text, Lang, Voice) ->
    aplay(text_to_wave(Text, Lang, Voice), #{}).
say(Text, Lang, Voice, Params) when is_map(Params) ->
    aplay(text_to_wave(Text, Lang, Voice), Params).

aplay(Wave) ->
    aplay(Wave, #{}).
aplay({Params, Samples}, Params0) ->
    case file:open(Samples, [ram, read, binary]) of
	{ok,Fd} ->
	    Params1 = maps:merge(Params, Params0),
	    try alsa_playback:fd(Fd, Params1) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	{error,Reason} ->
	    {error, file:format_error(Reason)}
    end.
	   
