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
-export([aplay/1, aplay/2, aplay/3]).
-export([aplay_wave/1]).
-type wave_header() ::
	{WaveType::atom(), SampleRate::number(),
	 NumChannels::integer(), ChannelMap::integer()}.

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

aplay(Text) ->
    aplay_wave(text_to_wave(Text)).
aplay(Text,Voice) ->
    aplay_wave(text_to_wave(Text,Voice)).
aplay(Text, Lang, Voice) ->
    aplay(text_to_wave(Text, Lang, Voice)).

%% utility using aplay!
aplay_wave({#{
	      format := Format,
	      sample_rate := SampleRate,
	      num_channels := NumChannels,
	      num_samples  := NumSamples
	     },Samples}) ->
    Command = lists:flatten(
		["aplay ",
		 "-r ",integer_to_list(SampleRate),$\s,
		 "-t raw ",
		 "-c ",integer_to_list(NumChannels),$\s,
		 "-s ",integer_to_list(NumSamples),$\s,
		 "-f ", string:to_upper(atom_to_list(Format)),$\s]),
    %% io:format("run command: ~s\n", [Command]),
    Aplay = erlang:open_port({spawn, Command}, [eof]),
    Aplay ! {self(), {command, Samples}},
    receive
	{Aplay, eof} ->
	    erlang:port_close(Aplay),
	    ok
    end.

