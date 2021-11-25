# flite

Erlang binding to flite.

## Require

Require flite-dev to be installed.

    sudo apt install flite1-dev

## usage

    flite:list\_voices().
	> [{"time","awb"},{"us","awb"},{"us","kal"},{"us","kal16"},
	   {"us","rms"},{"us","slt"}]
	
	flite:text_to_wave("Hello").
	> 
	{#{channel_map => {mono},format => s16_le,
       num_channels => 1,
	   num_samples => 16240,
	   sample_rate => 16000},
    <<...>>}

	flite:text_to_wave("Hello", "us", "awb").
	> 
	{#{channel_map => {mono},format => s16_le,
       num_channels => 1,
	   num_samples => 16240,
	   sample_rate => 16000},
    <<...>>}	
  
    flite:text_to_wave(Text::iolist() [Lang::string()] [Voice::string()]

    flite:aplay("Hello").
