# RuudsModPlayer
Amiga Module soundfile player.

This is my personal attempt to create a Mod Player with Lazarus-IDE and FPC (FreePascal).
It's a basic player which currently has the following functionality:

- Only Plays 4 channel files;
- Plays original format files;
- Plays M.K., M!K! and such files;
- Outputs in stereo to the soundcard using a samplerate you can specify (changeable only when no song is currently playing);
- You can specify if the player should use NTSC or PAL timing (on the fly while playing);
- You can output to canonical wave files;
- You can view the file content (samples, all pattern tables, etc)
- You can show debug output onscreen while playing a song (tracker info);
- You can play the raw samples on a fixed speed;
- You can specify where to start the song (handy for playing i.e. game sound effects which might be included in some files).

I guess that's about it for now.


Rudolf Cornelissen.
