# waveoutproject
minimal real-time synth with real-time recording/playback and midi input

The purpose of this project was to have a minimal real-time synthesizer and then be able to record keystrokes with millisecond precission and play this back, and also play along already recorded tracks. I.e. facilitate improvisation, sketches, however you might call this. It turned out that a midi keyboard is really good for this, so i bought a cheap one and included that functionality as well.

Luckily for the real-time synth I found something existing already: sonant. It turned out so good that I replicated it one to one. Which rarely ever happens to me. Normally I'm like the complete opposite: I find the existing stuff stupid and dumb and want to improve it. Not with sonant. That means something. I talked with the author personally and he allowed me to use it in this form. Thanks again. Btw that was some five years ago.

Next steps would be to add functionality for midi drum sets, i.e. bind certain midi codes to certain instruments like drum, base, hihat, etc.
Also more copy/paste functionality and maybe a "snap to grid" to get exact note values (eighth, sixteenth, etc), although this goeas a little bit against the original philosophy.

There is a way to get the demo-songs from the sonant page into this program. If anyone is interested I can do this.

OOPS: currently the midi functionality is commented out. I hope in the next days I will find a way to put both in: midi and computer keyboard keys...
