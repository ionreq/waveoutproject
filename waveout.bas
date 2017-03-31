/' waveout

	the synthesizer implementation is an exact replica of
	Sonant - Synth system targeted for 4k intros.
	(C) 2008-2009 Jake "Ferris" Taylor
	used with permission under Creative Commons terms


	esc		quit
	^			show/hide instrument panel
	end		save instrument to bank
	pgup		insert sample
	pgdown	delete sample
	ctrl		wheel=1     1*rowlen
	shift		wheel=10    8*rowlen
	alt		wheel=100   32*rowlen
	tab		wheel=1000  256*rowlen
	backspc	delete selection
	delete	silence selection


	song format:
		rowlen
		number of instruments
		this much instrument names from the bank file
		events
		tend, zeros

	instrument bank format:
		name
		values
		empty line


  evtl noch zu machen

	instrumente und oktaven für die beiden keyboards mit einem klick anwählbar
	farbige buttons für die instrumente
	darstellung in notenlinien-schreibweise
	noten mit der maus zeichnen, auch während play
		# und b irgendwie graphisch, zb mit hell/dunkel
	schlagzeug instrumente evtl noch auf andere tasten legen
		und dann eine feste frequenz dafür festlegen
	nochmal überprüfen was es mit dem song.note kommentar
		in waveout51.bas auf sich hat(tte)
	abfragen, ob lag größer als buffersize (va für sch)
	doch nochmal mit einer plag geschwindigkeit was überlegen
		damit es nicht bei jedem ruckler in plag gleich zu einem ruckler im stück kommt
	noten mit der maus verschieben/verändern/löschen
	in layers aufnehmen/editieren, einzelne layers anwählbar
	editier-filter, zb nur instrument x
	markierten bereich löschen: auch frequenzen beachten
	wenn in button geklickt, diesen button merken, und mausposition nur
		auf diesen beziehen
	instrument wellenform anzeigen (wenn taste gedrückt)
		evtl sogar einmal normal und einmal fft
	riesige liste mit instrumenten auf dem bildschirm,
		wenn man mit der maus darauffährt und eine taste drückt
		erklingt das instrument. mit klick wählt man es aus
		in die liste der song-instrumente
	wenn man auf instbutton ist, kein songrec. das ist gut zum ausprobieren.
		wenn dieses feature sich nicht so ergeben würde, müsste man es glatt einführen.
	beliebige anzahl instrumente, "neues instrument" button
	option, die instrumente auch im alten format direkt im song zu speichern


  gemacht

	tasten machen nur frequenzen
	oszillatoren unabhängig von den tasten machen
	instrumente mit verschiedenen wellenformtypen
	zwei unabhängige keyleisten mit unabhängig einstellbaren oktaven und instrumenten
	volume in instrument rein
	aufnahmefunktion
	wiedergabe von aufgenommenem
	bei wiedergabe gleichzeitig aufnehmen
	farben für die instrumente
	endmarkierung für song
	songrec in songplay einfügen nach zeiten geordnet
	song speichern und laden
	graphik mit opengl, rechtecken und antialiasing
	songcounter unabhängig von waveoutcounter pt machen
	stop (kein play, kein scrolling)
	ab beliebiger stelle abspielen
	zoom für songdarstellung
	im song herumscrollen (mit maus verschieben)
	nur aufnehmen wenn play=1
	schalterflächen größer und mit rahmen rum
	mit strg und maus bereich markieren
	markierten bereich löschen (noch ohne frequenzen)
	alles nach cursor (dh playcnt) nach links/rechts verschieben
	fft
	delay, echo, reverb
	mehrere oszillatoren pro instrument
	attack, decay, sustain, release in instrument rein
	filter ala sonant in instrument rein
	delay für echo in instrument rein
	voices in vier threads rechnen lassen
	sonant: ein delay buffer pro instrument, nicht pro voice
	für pan und lfo und sachen die von p abhängig sind:
		playcnt verwenden statt pt, damit es immer gleich ist
		unabhängig von der startzeit. pt ist nur für echtzeit einspielen!
	zum testen ein grosses wav file machen, einmal mit testsonant.exe
		und einmal mit waveout. mit audacity vergleichen, ob wellenform
		gleich bzw stark ähnlich. evtl single statt double benutzen.
		probieren, es bit für bit gleich hinzubekommen.
		(geht nicht wegen rand(): die reihenfolge der aufrufe ist unterschiedlich)
	3d graphische anzeigeform
	mit mehr als einem thread funktioniert es noch nicht
		etwas überlegen, wie man song wieder im hauptthread lesen
		kann, und die voices trotzdem sample-genau einschalten kann
		lösung: voice buffer, dh alle voices ca 1000 samples lang buffern
	anzeigen, wie weit voi hinter schedule und waveout hinter voi
	buttons als opengl drawlist (nix gebracht)
	instrument editor
	für vlag und wlag geschwindigkeit einführen, dh eine art WL
		anzahl samples die jeden durchgang gemacht wird, und
		diese anzahl dann nur um eins erhöhen oder erniedrigen
		jenachdem ob sie zu groß oder zu klein ist
		das sollte eine flüssigere anzeige ergeben
		und rucklerer verhindern
		(probiert, hat nix gebracht bzw nicht funktioniert)
	fx filter wirkung mit showfft ausprobieren
	instrument panel ein/ausblendbar machen
	songrec in eigenen (helleren) farben anzeigen
	was überlegen wie man die envelope werte feiner einstellen kann
	shift taste für zehnerschritte bei envelope
	gehaltene instrumente (neuen schalter einführen, .press abfragen)
	buttons die nicht viele werte haben schmaler und nebeneinander
	farbe des instruments im panel anzeigen
	drawsong(): das aktuelle keyboard instrument (1 oder instshow)
		wird im vordergrund angezeigt (dh als letztes gezeichnet)
		damit es nicht von den anderen instrumenten übermalt wird
	bpm button
	clear song button
	mergen von songrec zu songplay mit button
	scheduler routine wartet auf 1ms multimedia timer
	getfrequency raus. dabei entdeckt, daß es songs gibt, in denen
		die maximalwerte nicht eingehalten werden
		zb regressions hatte pan_freq=8, was durch getfrequency zu 0 wurde.
		beatnik hat delay_time=9, viele haben fx_freq=11025.
		liver hatte auch pan_freq=8. geändert zu 0
	im song instrumente nach namen laden
	beliebige anzahl instrumente
	namen für instrumente und als extra dateien abspeichern
	instrumentenbank datei mit allen instrumenten
	instrument namen button mit wheel funktion, entkoppeln von kinst
	instrument in bank speichern, bank in datei speichern
	ein modifiziertes instrument (werte geändert)
		kriegt beim speichern einen anderen namen (+"_mod")
		und wird an das ende der bank kopiert, nbankinst+=1
	"_mod" kriegt noch eine laufende nummer
	tab taste für 1000er inkrement
	vertical sync mit directdraw funktion! wie in guten alten zeiten!
	button für playcnt
	load und save mit GetOpenFileName()
		wie kann man den dialog auch im fullscreen modus anzeigen?
			antwort: momentan gar nicht, es ist ein opengl problem unter win7
			workaround: fenster minimieren und dann wieder maximieren
		wie kann man die threads anhalten?
			evtl - ist sonst nicht so schlimm, weil ja nur die
			notentasten im thread abgefragt werden, keine funktionstasten
			evtl kann man auch einfach waveoutpause() machen
	für playcnt button eigene wheel-schrittweiten: vielfache von rowlen
	frequenzen aus getnotefreq() an kammerton 440 Hz angepasst
	tasten anders, sodaß mehr oktaven draufpassen


  noch zu machen
	beim laden eines songs playcnt=0 und ep=1 setzen
	wenn man play drückt ohne zu mergen ist songrec weg:
		ist das gut (ein feature) oder schlecht?
	was ist, wenn man selectbox nach links zieht? ist dann breite<0?
	auto insert pattern: solange play/rec wird das pattern im copybuffer
		automatisch immmer wieder angefügt
	loop-aufnahme: altes wird überschrieben (wenn taste gedrückt)
	loop-wiedergabemodus
	cut/copy/paste dh copybuffer einführen
	nur in songrec editieren (für insdel ist schon, noch für alles andere)
	für snaptogrid: strg=64stel, shift=32stel, alt=16tel, tab=8tel (oder so)
	für alle funktionstasten: gedrückt counter,
		und dann ein counter wert, ab dem wiederholung eintritt
		die wiederholungsgeschwindigkeit kann evtl je nach auflösung sein
	taste (pos1) für abspeichern von instrument in bank ohne "_modx"

'/


#Include "string.bi"
#Include "fbgfx.bi"
#Include "gl/gl.bi"
#Include "gl/glu.bi"
#Include "windows.bi"
#Include "win/mmsystem.bi"
#Include "win/ddraw.bi"
#Include "win/commdlg.bi"
Const WAVE_MAPPER = -1

Const PI = 6.283185307


' file

'Const LOADSONGNAME = "song_ambidumbi.txt"
'Const LOADSONGNAME = "song_beatnik.txt"
'Const LOADSONGNAME = "song_chill.txt"
'Const LOADSONGNAME = "song_chippy.txt"
'Const LOADSONGNAME = "song_fabrik.txt"
'Const LOADSONGNAME = "song_frank4k.txt"
'Const LOADSONGNAME = "song_haumeadrums.txt"
'Const LOADSONGNAME = "song_jajajaja.txt"
'Const LOADSONGNAME = "song_liver.txt"
'Const LOADSONGNAME = "song_microscope.txt"
'Const LOADSONGNAME = "song_poseidon.txt"
'Const LOADSONGNAME = "song_regressions.txt"
'Const LOADSONGNAME = "song_synth4k.txt"
Const LOADSONGNAME = "loadsong.txt"
Const SAVESONGNAME = "savesong.txt"
Const LOADINSTRUMENTBANKNAME = "instrumentbank.txt"
Const SAVEINSTRUMENTBANKNAME = "instrumentbank.txt"

Dim Shared As Integer modcnt		' counter for "_modx" appendix
Dim Shared As String progdir		' program path used for bank loading and saving


' graphics

Const SX = 1350	' screen size
Const SY = 700
'Const SX = 1024-8	' screen size
'Const SY = 768-32

Const FY = SY		' height of song
Const GY = FY/(8*12+6)		' height of notes

Dim Shared As Integer playdy		' y translation of song (playcnt is x translation)
Dim Shared As Double zoom			' zoom (default 1)
Dim Shared As HWND hwnd


' waveout

Const SPS = 44100				' samples per second
Const CH = 2					' number of channels
Const BITS = 16				' bits per sample
Const BYTES = BITS/8			' bytes per sample
Const BSIZE = 65536			' buffer size in words
'Const LAT = 4000				' latency in samples (for fft)
Const LAT = 3000				' latency in samples
Const CLIPVALUE = 32767		' audio clip value
Const WRITEWAV = 0			' 1: write raw data of loaded song. attention: endsong must not change!

Const OUTPUTVOLUME = 0.1

Dim Shared As HWAVEOUT hWaveOut		' waveout structures
Dim Shared As WAVEFORMATEX wfx
Dim Shared As WAVEHDR header

Dim Shared As Short buffer(BSIZE)	' waveout buffer

Dim Shared As Integer play			' play or halt
Dim Shared As Integer playcnt		' current play position
Dim Shared As Integer endsong		' endtime of playsong

Dim Shared As Integer pt, pn, pl, pg		' current sample position in buffer
Dim Shared As Integer plag, vlag, wlag		' lag counters
Dim Shared As Integer pc, pcstart			' screen synced sample position for smooth scrolling of graphics
Dim Shared As Double pcdouble		' screen synced sample position

Dim Shared As Short Ptr recbuf		' buffer for raw data recording
Dim Shared As Integer recbuflen		' length of raw buffer (fixed, determined from songlength)


' mouse

Dim Shared As Integer holdl, holdr, firstl, firstr, lastl, lastr	' mouse button
Dim Shared As Integer oldwheel, wheel		' mouse wheel
Dim Shared As Integer mx, mplaycnt, my, mplaydy, pangrab		' for panning


' keys

Const NKBKEYS1 = 5+7+5+1
Const NKBKEYS2 = 7+5+7+1
Const NKBKEYS = NKBKEYS1+NKBKEYS2		' number of keys with notes

Dim Shared As Integer keys(NKBKEYS) => {  _		' virtual key code of note keys
226, VK_A, VK_Y, VK_S, VK_X,  VK_C, VK_F, VK_V, VK_G, VK_B, VK_H, VK_N,  VK_M, VK_K, 188, VK_L, 190,  189,  _
VK_Q, VK_2, VK_W, VK_3, VK_E, VK_4, VK_R,  VK_T, VK_6, VK_Z, VK_7, VK_U,  VK_I, VK_9, VK_O, VK_0, VK_P, 219, 186,  187 _
}

Const NMIDIKEYS1 = 2*12
Const NMIDIKEYS2 = 2*12+1
Const NMIDIKEYS = NMIDIKEYS1+NMIDIKEYS2

Const NKEYS = NKBKEYS+NMIDIKEYS

Const NOTHKEYS = 2		' number of keys with other functions

Dim Shared As Integer othkeys(NOTHKEYS) => { 220, VK_END }	' virtual key codes

Namespace nsothkey		' other keys than note playing
Enum
showinst = 0
saveinst
End Enum
End Namespace

Type tkey
	act As Integer		' current key pressed status
	last As Integer	' last key pressed status
	note As Integer	' note the key is playing
	inst As Integer	' instrument the key is playing
	voi As Integer		' voice the key is on
	ee As Integer		' event number generated when the key was pressed
End Type

Dim Shared As tkey key(NKEYS), othkey(NOTHKEYS)		' note keys and other keys

Dim Shared As Integer kinst1, kinst2	' key instruments
Dim Shared As Integer koct1, koct2		' key octaves

Dim Shared As Integer up, down	' key up/down event counter
Dim Shared As Integer control, shift, altkey, tabkey		' keys pressed or not

Dim Shared As Integer midikey(128)


' instruments

Type toscillator		' oscillator
	octv As UByte
	det As UByte
	detune As UByte
	xenv As UByte
	vol As UByte
	waveform As UByte
End Type

Type tnoise				' noise
	fader As UByte
End Type

Type tenvelope			' envelope
	attack As Integer
	sustain As Integer
	release As Integer
	master As Integer
	hold As Integer
End Type

Type teffects			' effects
	filter As UByte
	freq As Double
	resonance As UByte
	delay_time As UByte
	delay_amt As UByte
	pan_freq As UByte
	pan_amt As UByte
End Type

Type tLFO				' LFO
	osc1_freq As UByte
	fx_freq As UByte
	freq As UByte
	amt As UByte
	waveform As UByte
End Type

Const NINSTBANKMAX = 1000		' max number of instruments in bank
Const NINSTMAX = 8				' max number of instruments in song

Type instrument
	nam As String			' instrument name
	banknum As Integer	' number in instrument bank. given while loading. used to display name
	osc1 As toscillator
	osc2 As toscillator
	noise As tnoise
	env As tenvelope
	fx As teffects
	lfo As tLFO
End Type

Dim Shared As instrument inst(NINSTMAX), instbank(NINSTBANKMAX)
Dim Shared As Integer ninst			' current number of instruments in song
Dim Shared As Integer ninstbank		' number of loaded bank instruments

Type tcolor
	As Double r,g,b
End Type

Dim Shared As tcolor farben(NINSTMAX) => { (0,0,0), _		' instrument colors
(1,0,0), (0.9,0.4,0), (1,1,0), (0,1,0), _
(0,1,1), (0,0.3,1), (0.9,0,0.9), (0.5,0.5,0.5) _
}

Dim Shared As Integer instpanelshow		' display instrument panel or not
Dim Shared As Integer panelinstnum		' panel instrument number


' threads

Const NTHR = 2				' number of threads for voices
Const DBSIZE = 60000		' delay buffer size in samples

Type tthread
	pt As Integer	' pt for each thread
	dbufl(NINSTMAX,DBSIZE) As Double
	dbufr(NINSTMAX,DBSIZE) As Double		' delay buffer per inst
	dbwp As Integer			' delay buffer wrtie position
	playcnt As Integer		' local playcnt for thread
End Type

Dim Shared As tthread thr(NTHR)		' voice threads


' voices

Const NVPERT = 16					' number of voices per thread
Const NVOICES = NTHR*NVPERT	' number of voices that can sound at the same time

Type voice
	free As Integer		' is voice free or occupied
	press As Integer		' is key pressed
	inst As Integer		' instrument
	note As Integer		' note
	c As Integer			' sample counter
	c1 As Double			' osc1 phase
	c2 As Double			' osc2 phase
	low As Double			' filter state
	band As Double
	high As Double
	osc1f As Double
	osc2f As Double
	lfof As Double
	panf As Double
	attack As Integer
	sustain As Integer
	release As Integer
	q As Double
End Type

Dim Shared As voice voi(NVOICES)		' current voice line


' schedule

Const SCHSIZE = 500		' schedule buffer size

Type schedule
	inst As Integer		' instrument
	note As Integer		' note
	press As Integer		' is key pressed
	ee As Integer			' event number playing
End Type

Dim Shared As schedule schbuf(NVOICES,SCHSIZE)	' buffered schedule lines
Dim Shared As schedule sch(NVOICES)		' current schedule line
Dim Shared As Integer vp		' schedule pointer


' timer events, handles, etc

Dim Shared As HANDLE htmr		' 1 ms waitable timer handle
Dim Shared As HANDLE hevtsch(NTHR), hevtvoi(NTHR)		' events for schedule ready and voices ready


' song events

Const NMAXEVENTS = 10000		' determines max song length
Const TEND = 2^29					' end marker for song (in t field)

Type event
	t As Integer		' time
	press As Integer	' press or release
	inst As Integer	' instrument
	note As Integer	' note number
	ee As Integer		' for press/release pointer to corresponding release/press event
End Type

Dim Shared As event songrec(NMAXEVENTS), songplay(NMAXEVENTS)		' recorded song, played song
Dim Shared As Integer er, ep		' current rec/play position
Dim Shared As Integer rowlen		' for sonant import (length of a row in samples)
Dim Shared As Integer bpm			' beats per minute (derived from rowlen)


' buttons

Namespace nsbut		' button names
Enum
play=1, load, save, bpm, clr, merge, playcnt
koct1, kinst1, koct2, kinst2
inst_num, inst_name
osc1_vol, osc1_oct, osc1_det, osc1_detune, osc1_xenv, osc1_waveform
osc2_vol, osc2_oct, osc2_det, osc2_detune, osc2_xenv, osc2_waveform
noise_fader
env_attack, env_sustain, env_release, env_master, env_hold
lfo_osc1_freq, lfo_fx_freq, lfo_freq, lfo_amt, lfo_waveform
fx_filter, fx_freq, fx_resonance, fx_delay_time, fx_delay_amt, fx_pan_freq, fx_pan_amt
nbuttons
End Enum
End Namespace

Const NBUTTONS = nsbut.nbuttons-1		' number of buttons

Type button
	x As Integer		' position of bottom left corner (opengl coordinates)
	y As Integer
	b As Integer		' width
	h As Integer		' height
	text As String		' text
	p As Double			' slider position
	m As Double			' max value
	show As Integer	' show or hide the button
End Type

Dim Shared As button but(NBUTTONS)


' selectbox

Dim Shared As Integer selectbox
Dim Shared As Integer selectx, selecty, selectb, selecth


' fft

Const FFTSWITCH = 0		' turn on for fft

Const BX = 12     ' number of bits of the buffer size
Const NX = 2^BX   ' size of real buffer

Type complex
	r As Double
	i As Double
End Type

Const AA = 0		' array
Const AN = 1		' inverse transformed array
Const ARB = 2

Const AF = 0		' array Fourier transformed
Const FFT0 = 1		' intermediate buffers for the stages
Const FFT1 = 2		' (toggle between them)
Const AFB = 3		' number of Fourier buffers

Dim Shared As Double fftr(NX,ARB)		' real arrays
Dim Shared As complex fftc(NX/2+1,AFB)	' complex arrays
Dim Shared As Double px1(NX/2+1,BX,2), px3(NX/2+1,BX,2)	' plans for fft
Dim Shared As complex px2(NX/2+1,BX,2)


Declare Sub main ()
main ()
End


' fft
'
Private Function cmplx (r As Double, i As Double) As complex
	Dim As complex c
	c.r = r
	c.i = i
	Return c
End Function


Private Function cscl (a As complex, s As Double) As complex
	Dim As complex c
	c.r = s*a.r
	c.i = s*a.i
	Return c
End Function


Private Function cmul (a As complex, b As complex) As complex
	Dim As complex c
	c.r = a.r*b.r - a.i*b.i
	c.i = a.r*b.i + a.i*b.r
	Return c
End Function


Private Function cadd (a As complex, b As complex) As complex
	Dim As complex c
	c.r = a.r + b.r
	c.i = a.i + b.i
	Return c
End Function


Private Function csub (a As complex, b As complex) As complex
	Dim As complex c
	c.r = a.r - b.r
	c.i = a.i - b.i
	Return c
End Function


Private Function cexp (a As Double) As complex
	Dim As complex c
	c.r = Cos(a)
	c.i = Sin(a)
	Return c
End Function


Private Function ccnj (a As complex) As complex
	Dim As complex c
	c.r = a.r
	c.i = -a.i
	Return c
End Function


Private Sub copybufferrc (vo As Integer, na As Integer)
	Dim As Integer x

	For x = 0 To NX/2-1
		fftc(x,na).r = fftr(2*x+0,vo)
		fftc(x,na).i = fftr(2*x+1,vo)
	Next
End Sub


Private Sub copybuffercr (vo As Integer, na As Integer)
	Dim As Integer x

	For x = 0 To NX/2-1
		fftr(2*x+0,na) = fftc(x,vo).r
		fftr(2*x+1,na) = fftc(x,vo).i
	Next
End Sub


Private Function bitreverse (m As Integer, b As Integer) As Integer
	Dim As Integer c, t

	c = 0
	For t = 0 To b-1
		c = (c Shl 1) Or ((m Shr t) And 1)
	Next
	Return c
End Function


Private Sub fft1d_planx (eb As Integer, si As Integer)
	Dim As Integer l, j, x, s
	Dim As Double w

	s = (si+1)/2
	For x = 0 To NX/2
		If si=1 And eb=0 Then

			px1(x,eb,s) = x
			px3(x,eb,s) = NX/2-x
			px2(x,eb,s) = cexp(si*pi*(x/NX+1/4))

		ElseIf si=-1 And eb=BX-1+1 Then

			If x=0 Or x=NX/2 Then
				px1(x,eb,s) = 0
				px3(x,eb,s) = 0
			Else
				px1(x,eb,s) = x
				px3(x,eb,s) = NX/2-x
			EndIf
			px2(x,eb,s) = cexp(si*pi*(x/NX+1/4))

		ElseIf x<NX/2 Then

			l = 2^eb
			j = x Mod l
			If j<l/2 Then
				If eb=1 Then
					px1(x,eb,s) = bitreverse(x    ,BX-1)
					px3(x,eb,s) = bitreverse(x+l/2,BX-1)
				Else
					px1(x,eb,s) = x
					px3(x,eb,s) = x+l/2
				EndIf
				w = si*pi*j/l
				px2(x,eb,s).r = Cos(w)
				px2(x,eb,s).i = Sin(w)
			Else
				If eb=1 Then
					px1(x,eb,s) = bitreverse(x-l/2,BX-1)
					px3(x,eb,s) = bitreverse(x    ,BX-1)
				Else
					px1(x,eb,s) = x-l/2
					px3(x,eb,s) = x
				EndIf
				w = si*pi*(j-l/2)/l
				px2(x,eb,s).r = -Cos(w)
				px2(x,eb,s).i = -Sin(w)
			EndIf

		EndIf
	Next
End Sub


Private Sub fft1d_stage (d As Integer, eb As Integer, si As Integer, vo As Integer, na As Integer)
	Dim As Integer x, s
	Dim As complex b, c

	s = (si+1)/2
	For x = 0 To NX/2

		If si=1 And eb=0 Then

			b = fftc(px1(x,eb,s),vo)
			c = ccnj(fftc(px3(x,eb,s),vo))
			b = cadd(cadd(b,c),cmul(csub(b,c),px2(x,eb,s)))
			fftc(x,na) = cscl(b,0.5*Sqr(2))

		ElseIf si=-1 And eb=BX-1+1 Then

			b = fftc(px1(x,eb,s),vo)
			c = ccnj(fftc(px3(x,eb,s),vo))
			b = cadd(cadd(b,c),cmul(csub(b,c),px2(x,eb,s)))
			fftc(x,na) = cscl(b,0.5/Sqr(2))

		ElseIf x<NX/2 Then

			b = cadd (fftc(px1(x,eb,s),vo), cmul (px2(x,eb,s), fftc(px3(x,eb,s),vo)))
			fftc(x,na) = cscl(b,1/Sqr(2))

		EndIf

	Next

End Sub


Private Sub fft1d (vo As Integer, na As Integer, si As Integer)
	Dim As Integer t, fftcurrent, fftother

	fftcurrent = FFT0
	fftother = FFT1

	If si=-1 Then

		copybufferrc (vo, fftcurrent)
		For t = 1 To BX-1+1
			If t=BX Then
				fft1d_stage (1, t, si, fftcurrent, na)
			Else
				fft1d_stage (1, t, si, fftcurrent, fftother)
			EndIf
			Swap fftcurrent, fftother
		Next

	Else

		For t = 0 To BX-1
			If t=1 Then
				fft1d_stage (2, t, si, vo, fftother)
			Else
				fft1d_stage (2, t, si, fftcurrent, fftother)
			EndIf
			Swap fftcurrent, fftother
		Next
		copybuffercr (fftcurrent, na)

	EndIf

End Sub


Sub fft_init ()
	Dim As Integer t

	For t = 0 To BX-1+1
		fft1d_planx (t, -1)
		fft1d_planx (t, 1)
	Next
End Sub


' save the whole instrument bank to a file
'
Sub saveinstrumentbank (fn As String)
	Dim As Integer t

	Open fn For Output As #1

	For t = 1 To ninstbank
		Print #1, instbank(t).nam

		Print #1, instbank(t).osc1.octv;" ";
		Print #1, instbank(t).osc1.det;" ";
		Print #1, instbank(t).osc1.detune;" ";
		Print #1, instbank(t).osc1.xenv;" ";
		Print #1, instbank(t).osc1.vol;" ";
		Print #1, instbank(t).osc1.waveform

		Print #1, instbank(t).osc2.octv;" ";
		Print #1, instbank(t).osc2.det;" ";
		Print #1, instbank(t).osc2.detune;" ";
		Print #1, instbank(t).osc2.xenv;" ";
		Print #1, instbank(t).osc2.vol;" ";
		Print #1, instbank(t).osc2.waveform

		Print #1, instbank(t).noise.fader

		Print #1, instbank(t).env.attack;" ";
		Print #1, instbank(t).env.sustain;" ";
		Print #1, instbank(t).env.release;" ";
		Print #1, instbank(t).env.master;" ";
		Print #1, instbank(t).env.hold

		Print #1, instbank(t).fx.filter;" ";
		Print #1, instbank(t).fx.freq;" ";
		Print #1, instbank(t).fx.resonance;" ";
		Print #1, instbank(t).fx.delay_time;" ";
		Print #1, instbank(t).fx.delay_amt;" ";
		Print #1, instbank(t).fx.pan_freq;" ";
		Print #1, instbank(t).fx.pan_amt

		Print #1, instbank(t).lfo.osc1_freq;" ";
		Print #1, instbank(t).lfo.fx_freq;" ";
		Print #1, instbank(t).lfo.freq;" ";
		Print #1, instbank(t).lfo.amt;" ";
		Print #1, instbank(t).lfo.waveform

		Print #1,
	Next

	Close #1
End Sub


' save the playsong
'
Sub savesong (songname As String)
	Dim As Integer t

	Open songname For Output As #1

	' sonant
	Print #1, rowlen
	Print #1, ninst

	For t = 1 To ninst
		Print #1, inst(t).nam
	Next

	For t = 1 To NMAXEVENTS
		Print #1, songplay(t).t;
		Print #1, songplay(t).press;
		Print #1, songplay(t).inst;
		Print #1, songplay(t).note;
		Print #1, songplay(t).ee
		If songplay(t).t=TEND Then Exit For
	Next

	Close #1
End Sub


' load all instruments from the bank
'
Sub loadinstrumentbank (fn As String)
	Dim As Integer t, emptyline

	Open fn For Input As #1

	ninstbank = 0
	For t = 1 To NINSTBANKMAX
		If EOF(1) Then Exit For
		Input #1, instbank(t).nam
		If EOF(1) Then Exit For
		instbank(t).banknum = t

		Input #1, instbank(t).osc1.octv
		Input #1, instbank(t).osc1.det
		Input #1, instbank(t).osc1.detune
		Input #1, instbank(t).osc1.xenv
		Input #1, instbank(t).osc1.vol
		Input #1, instbank(t).osc1.waveform

		Input #1, instbank(t).osc2.octv
		Input #1, instbank(t).osc2.det
		Input #1, instbank(t).osc2.detune
		Input #1, instbank(t).osc2.xenv
		Input #1, instbank(t).osc2.vol
		Input #1, instbank(t).osc2.waveform

		Input #1, instbank(t).noise.fader

		Input #1, instbank(t).env.attack
		Input #1, instbank(t).env.sustain
		Input #1, instbank(t).env.release
		Input #1, instbank(t).env.master
		Input #1, instbank(t).env.hold

		Input #1, instbank(t).fx.filter
		Input #1, instbank(t).fx.freq
		Input #1, instbank(t).fx.resonance
		Input #1, instbank(t).fx.delay_time
		Input #1, instbank(t).fx.delay_amt
		Input #1, instbank(t).fx.pan_freq
		Input #1, instbank(t).fx.pan_amt

		Input #1, instbank(t).lfo.osc1_freq
		Input #1, instbank(t).lfo.fx_freq
		Input #1, instbank(t).lfo.freq
		Input #1, instbank(t).lfo.amt
		Input #1, instbank(t).lfo.waveform

		Input #1, emptyline
		ninstbank += 1
	Next

	Close #1
End Sub


' return an instruments bank number given it's name
' 0 if not found
'
Function findinstrument (s As String) As Integer
	Dim As Integer t
	For t = 1 To ninstbank
		If instbank(t).nam = s Then Return t
	Next
	Return 0
End Function


' set a song instrument to a bank instrument (copy it)
'
Sub setinstrument (i As Integer, s As String)
	Dim As Integer t
	t = findinstrument (s)
	If t>0 Then inst(i) = instbank(t)
End Sub


' load a song
'
Sub loadsong (songname As String)
	Dim As Integer t, d
	Dim As String s

	Open songname For Input As #1

	If Err>0 Then Exit Sub

	' only for sonant
	Input #1, rowlen
	bpm = Int(60*SPS/rowlen/4+0.5)

	Input #1, ninst

	For t = 1 To ninst
		Input #1, s
		setinstrument (t, s)
	Next

	For t = 1 To NMAXEVENTS
		Input #1, songplay(t).t
		Input #1, songplay(t).press
		Input #1, songplay(t).inst
		Input #1, songplay(t).note
		Input #1, songplay(t).ee
		If songplay(t).t=TEND Then Exit For
	Next

	' only for sonant
	' insert the event pointers
	'For t = 1 To NMAXEVENTS
	'	If songplay(t).t=TEND Then Exit For
	'	'songplay(t).t += 32*rowlen
	'	If songplay(t).press=1 Then
	'		For d = t+1 To NMAXEVENTS
	'			If songplay(d).press=0 And songplay(d).inst = songplay(t).inst Then
	'				songplay(d).ee = t
	'				songplay(t).ee = d
	'				Exit For
	'			EndIf
	'		Next
	'	EndIf
	'Next

	Close #1
End Sub


' determine length of the song
'
Function lengthsong () As Integer
	Dim As Integer t, e

	For t = 1 To NMAXEVENTS
		If songplay(t).t = TEND Then
			If t=1 Then
				e = 0
			Else
				e = songplay(t-1).t
			EndIf
			Exit For
		EndIf
	Next

	' only for sonant: round length to a multiple of 32*rowlen
	e = (Int(e/(32*rowlen))+1)*32*rowlen

	Return e
End Function


' merges songrec into songplay
'
Sub merge ()
	Dim As Integer t, d, i, u, er, ep, em
	Dim As event merken

	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).press = 1 Then
			d = songplay(ep).ee
			If songplay(d).press<>0 Or songplay(d).ee<>ep Then
				Open "d:\tmp\test.txt" For Output As #1
				Print #1, "davor"
				Print #1, ep, songplay(d).press, songplay(d).ee
				For i = 1 To 20
					Print #1, i, songplay(i).t, songplay(i).press, songplay(i).note, songplay(i).ee			
				Next
				End
			EndIf
		EndIf
		ep += 1
	Next


	i = NMAXEVENTS
	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).press = 1 Then
			d = songplay(ep).ee
			songplay(d).ee = i
			songplay(ep).ee = i
			i += 1
		EndIf
		ep += 1
	Next
	er = 1
	For t = 1 To NMAXEVENTS
		If songrec(er).t=TEND Then Exit For
		If songrec(er).press = 1 Then
			d = songrec(er).ee
			songrec(d).ee = i
			songrec(er).ee = i
			i += 1
		EndIf
		er += 1
	Next


	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).ee<NMAXEVENTS Then
			Open "d:\tmp\test.txt" For Output As #1
			Print #1, "aha play"
			Print #1,ep
			For i = 1 To 200
				Print #1, i, songplay(i).t, songplay(i).press, songplay(i).note, songplay(i).ee			
			Next
			End
		EndIf
		ep += 1
	Next
	er = 1
	For t = 1 To NMAXEVENTS
		If songrec(er).t=TEND Then Exit For
		If songrec(er).ee<NMAXEVENTS Then
			Open "d:\tmp\test.txt" For Output As #1
			Print #1, "aha rec"
			Print #1,er
			For i = 1 To 200
				Print #1, i, songrec(i).t, songrec(i).press, songrec(i).note, songrec(i).ee			
			Next
			End
		EndIf
		er += 1
	Next


	For d = 1 To NMAXEVENTS
		If songplay(d).t = TEND Then em = d : Exit For
	Next
	er = 1
	ep = 1
	For t = 1 To NMAXEVENTS
		If songrec(er).t=TEND Then Exit For
		If songrec(er).t<songplay(ep).t Then
			For d = em+1 To ep+1 Step -1
				songplay(d) = songplay(d-1)
			Next
			songplay(ep) = songrec(er)
			er += 1
			ep += 1
			em += 1
		Else
			ep += 1
		EndIf
	Next

	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).press=1 Then
			i = songplay(ep).ee
			For d = ep+1 To NMAXEVENTS
				If songplay(d).ee=i Then
					songplay(d).ee = ep
					songplay(ep).ee = d
					Exit For
				EndIf
			Next
		EndIf
		ep += 1
	Next


	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).ee>=NMAXEVENTS Then
			Open "d:\tmp\test.txt" For Output As #1
			Print #1, "aha danach"
			Print #1,ep
			For i = 1 To 200
				Print #1, i, songplay(i).t, songplay(i).press, songplay(i).note, songplay(i).ee			
			Next
			End
		EndIf
		ep += 1
	Next

	ep = 1
	For t = 1 To NMAXEVENTS
		If songplay(ep).t=TEND Then Exit For
		If songplay(ep).press = 1 Then
			d = songplay(ep).ee
			If songplay(d).press<>0 Or songplay(d).ee<>ep Then
				Open "d:\tmp\test.txt" For Output As #1
				Print #1, "danach"
				Print #1, ep, songplay(d).press, songplay(d).ee
				For i = 1 To 20
					Print #1, i, songplay(i).t, songplay(i).press, songplay(i).note, songplay(i).ee			
				Next
				End
			EndIf
		EndIf
		ep += 1
	Next

End Sub


' deletes in songplay everything in the selectbox
'
Sub selectiondelete (dt As Integer)
	Dim As Integer t, p, d
	Dim As event songcopy(NMAXEVENTS)

	' make a copy of songplay
	For t = 1 To NMAXEVENTS
		songcopy(t) = songplay(t)
		If songcopy(t).t = TEND Then Exit For
	Next

	' mark all events that are to be deleted
	For t = 1 To NMAXEVENTS
		If songcopy(t).t = TEND Then Exit For
		If songcopy(t).t>=selectx And songcopy(t).t<=selectx+selectb Then
			d = songcopy(t).ee
			songcopy(d).ee = -1
			songcopy(t).ee = -1
		EndIf
	Next

	' adjust times if destructive delete
	For t = 1 To NMAXEVENTS
		If songcopy(t).t = TEND Then Exit For
		If songcopy(t).t>=selectx+selectb Then
			songcopy(t).t -= dt
		EndIf
	Next

	' copy to result correcting event pointers
	p = 1
	For t = 1 To NMAXEVENTS
		If songcopy(t).ee<>-1 Then
			songplay(p) = songcopy(t)
			If songcopy(t).press=1 Then
				d = songcopy(t).ee
				songcopy(d).ee = p
			EndIf
			If songplay(p).press=0 Then
				d = songplay(p).ee
				songplay(d).ee = p
			EndIf
			p += 1
		EndIf
		If songcopy(t).t = TEND Then Exit For
	Next

	endsong = lengthsong ()
End Sub


' insert or delete space at playcnt
'
Sub insdelspace (a As Integer)
	Dim As Integer t, j

	j = 0
	For t = 1 To NMAXEVENTS
		'If songplay(t).t = TEND Then Exit For
		'If songplay(t).t>=playcnt Then j = 1
		'If j Then songplay(t).t += a
		If songrec(t).t = TEND Then Exit For
		If songrec(t).t>=playcnt Then j = 1
		If j Then songrec(t).t += a
	Next
End Sub


' test if voice is free
'
Function isfree (v As Integer) As Integer
	If voi(v).c>=voi(v).attack+voi(v).sustain+voi(v).release Then
		Return 1
	Else
		Return 0
	EndIf
End Function


' find a free, unused oscillator. if none, return 0
'
Function freevoice () As Integer
	Dim As Integer v

	For v = 1 To NVOICES
		If voi(v).free And isfree(v) And voi(v).inst=0 And sch(v).press=0 And voi(v).press=0 Then Return v
	Next
	Return 0
End Function


' determine key note and instrument
'
Sub initkeys ()
	Dim As Integer t

	For t = 0 To NKEYS-1
		If t<NKBKEYS1 Then
			key(t).note = t + koct1*12 + 87
			key(t).inst = kinst1
		ElseIf t<NKBKEYS1+NKBKEYS2 Then
			key(t).note = t + koct2*12 + 86-12
			key(t).inst = kinst2
		ElseIf t<NKBKEYS1+NKBKEYS2+NMIDIKEYS1 Then
			key(t).note = t-NKBKEYS + (koct1+7)*12 + 3
			key(t).inst = kinst1
		Else
			key(t).note = t-NKBKEYS + (koct2+7)*12 + 3
			key(t).inst = kinst2
		EndIf
	Next
End Sub


' clears the audio buffer
'
Sub clearbuffer ()
	Dim As Integer t
	For t = 0 To BSIZE-1
		buffer(t) = 0
	Next
End Sub


' clear the voices
'
Sub clearvoices ()
	Dim As Integer v
	For v = 1 To NVOICES
		voi(v).press = 0
		sch(v).press = 0
	Next
End Sub


' play the buffer
'
Sub startbuffer ()
	header.lpData = Cast (Any Ptr, @buffer(0))
	header.dwBufferLength = BSIZE*BYTES
	header.dwBytesRecorded = 0
	header.dwUser = 0
	header.dwFlags = WHDR_BEGINLOOP Or WHDR_ENDLOOP
	header.dwLoops = 65000
	header.lpNext = 0
	header.reserved = 0

	waveOutPrepareHeader (hWaveOut, @header, SizeOf(WAVEHDR))
	waveOutWrite (hWaveOut, @header, SizeOf(WAVEHDR))
End Sub


' returns the current waveout position in samples
'
Function getwaveoutposition () As Integer
	Dim As MMTIME mmtim
	mmtim.wType = TIME_SAMPLES
	waveOutGetPosition (hWaveOut, @mmtim, SizeOf(MMTIME))
	Return mmtim.sample
End Function


' draw the playsong and the recsong
'
Sub drawsongs ()
	Dim As Integer t, d, i, x, xe, y
	Dim As Double c, m, z
	Dim As Integer r(ninst)

	' determine display order for instruments, 1=front, 2=behind that, etc
	i = 3
	For t = 1 To ninst
		If t=kinst1 Then
			r(t) = 1
		ElseIf t=kinst2 Then
			r(t) = 2
		Else
			r(t) = i
			i += 1
		EndIf
	Next

	c = 1/SPS*100

	glMatrixMode (GL_PROJECTION)
	glLoadIdentity ()
	gluPerspective (90, SX/SY, 1, 10000)
	glMatrixMode (GL_MODELVIEW)
	glLoadIdentity ()

	glPushMatrix ()

	' glRotated (10, 1, 0, 0)	' oblique view
	glTranslated (-pc*c, -playdy*c, -SY/2*zoom)


	' draw the select box
	If selectbox Then
		m = 0.9
		glColor3d (1.0*(1-m)+0.8*m, 1.0*(1-m)+0.6*m, 1.0*(1-m)+0.4*m)
		glBegin (GL_QUADS)
		glVertex3d ( selectx*c         , -FY/2, 0)
		glVertex3d ((selectx+selectb)*c, -FY/2, 0)
		glVertex3d ((selectx+selectb)*c,  FY/2, 0)
		glVertex3d ( selectx*c         ,  FY/2, 0)
		glEnd ()
	EndIf

	' the beat lines
	glColor3d (0.9, 0.7, 0.5)
	For x = -8*rowlen To endsong+8*rowlen Step 8*rowlen
		glBegin (GL_QUADS)
		glVertex3d (x*c-0.5, -FY/2, 0.1)
		glVertex3d (x*c-0.5,  FY/2, 0.1)
		glVertex3d (x*c+0.5,  FY/2, 0.1)
		glVertex3d (x*c+0.5, -FY/2, 0.1)
		glEnd ()
	Next

	' the playcnt line
	glColor3d (1.0, 0.8, 0.6)
	glBegin (GL_QUADS)
	glVertex3d (pc*c-0.5, -FY/2, 0.2)
	glVertex3d (pc*c-0.5,  FY/2, 0.2)
	glVertex3d (pc*c+0.5,  FY/2, 0.2)
	glVertex3d (pc*c+0.5, -FY/2, 0.2)
	glEnd ()

	' the playsong
	For t = 1 To NMAXEVENTS
		If songplay(t).t = TEND Then Exit For
		If songplay(t).press=1 Then
			x = songplay(t).t
			If songplay(t).ee=0 Then
				xe = playcnt
			Else
				d = songplay(t).ee
				xe = songplay(d).t
			EndIf
			y = (songplay(t).note-87)*GY-FY/2
			i = songplay(t).inst
			m = 0.5
			glColor3d (0.8*m+(1-m)*farben(i).r, 0.6*m+(1-m)*farben(i).g, 0.4*m+(1-m)*farben(i).b)
			z = 9-r(i)
			glBegin (GL_QUADS)
			glVertex3d ( x*c, y   , z)
			glVertex3d (xe*c, y   , z)
			glVertex3d (xe*c, y+GY, z)
			glVertex3d ( x*C, y+GY, z)
			glEnd ()
		EndIf
	Next

	' the currently recorded pieces
	For t = 1 To er-1
		If songrec(t).press=1 Then
			x = songrec(t).t
			If songrec(t).ee=0 Then
				xe = playcnt
			Else
				d = songrec(t).ee
				xe = songrec(d).t
			EndIf
			y = (songrec(t).note-87)*GY-FY/2
			i = songrec(t).inst
			glColor3d (farben(i).r, farben(i).g, farben(i).b)
			z = 9-r(i)
			glBegin (GL_QUADS)
			glVertex3d ( x*c, y   , z)
			glVertex3d (xe*c, y   , z)
			glVertex3d (xe*c, y+GY, z)
			glVertex3d ( x*c, y+GY, z)
			glEnd ()
		EndIf
	Next

	glPopMatrix ()

	glMatrixMode (GL_PROJECTION)
	glLoadIdentity ()
	glOrtho (0, SX, 0, SY, -1, 1)
	glMatrixMode (GL_MODELVIEW)
	glLoadIdentity ()
End Sub


' print a text at a screen position using opengl calllists
'
Sub mytextout (x As Double, y As Double, z As Double, s As String)
	glRasterPos3d (x, y, z)
	glListBase (1000)
	glCallLists (Len(s), GL_UNSIGNED_BYTE, StrPtr(s))
End Sub


' initialize button positions and texts
'
Sub initbuttons ()
	Dim As button b
	Dim As instrument i
	Dim As Integer t
	Dim As String s

	If play Then s = "stop" : Else s = "play"
	b.x = 20*32 : b.y = 22*32-68 : b.h = 30 : b.b = 2*32 : b.text = s                : b.p = -1  : b.m = 0   : but(nsbut.play) = b
	b.x = 20*32 : b.y = 21*32-68 : b.h = 30 : b.b = 2*32 : b.text = "load"           : b.p = -1  : b.m = 0   : but(nsbut.load) = b
	b.x = 20*32 : b.y = 20*32-68 : b.h = 30 : b.b = 2*32 : b.text = "save"           : b.p = -1  : b.m = 0   : but(nsbut.save) = b

	b.x =  7*32 : b.y = 22*32-68 : b.h = 30 : b.b = 86   : b.text = "bpm: "+Str(bpm) : b.p = bpm : b.m = 256 : but(nsbut.bpm) = b
	b.x =  7*32 : b.y = 21*32-68 : b.h = 30 : b.b = 86   : b.text = "clear song"     : b.p = -1  : b.m = 0   : but(nsbut.clr) = b
	b.x =  7*32 : b.y = 20*32-68 : b.h = 30 : b.b = 2*32 : b.text = "merge"          : b.p = -1  : b.m = 0   : but(nsbut.merge) = b

	b.x = 11*32 : b.y = 22*32-68 : b.h = 30 : b.b = 256 : b.text = "..." : b.p = playcnt : b.m = endsong : but(nsbut.playcnt) = b

	b.x = 23*32 : b.y = 22*32-68 : b.h = 30 : b.b = 3*32 : b.text = "koct2: "+Str(koct2)   : b.p = koct2    : b.m = NINSTMAX : but(nsbut.koct2) = b
	b.x = 27*32 : b.y = 22*32-68 : b.h = 30 : b.b = 3*32 : b.text = "kinst2: "+Str(kinst2) : b.p = kinst2-1 : b.m = NINSTMAX : but(nsbut.kinst2) = b
	b.x = 23*32 : b.y = 21*32-68 : b.h = 30 : b.b = 3*32 : b.text = "koct1: "+Str(koct1)   : b.p = koct1    : b.m = NINSTMAX : but(nsbut.koct1) = b
	b.x = 27*32 : b.y = 21*32-68 : b.h = 30 : b.b = 3*32 : b.text = "kinst1: "+Str(kinst1) : b.p = kinst1-1 : b.m = NINSTMAX : but(nsbut.kinst1) = b

	t = panelinstnum
	i = inst(t)

	b.x = 33*32 : b.y = 32*20 : b.h = 18 : b.b =  32 : b.text = Str(t)+":" : b.p = t-1 : b.m = NINSTMAX : but(nsbut.inst_num) = b
	b.x = 35*32 : b.y = 32*20 : b.h = 18 : b.b = 192 : b.text = i.nam : b.p = i.banknum-1 : b.m = ninstbank : but(nsbut.inst_name) = b

	b.x = 33*32 : b.y = 30*20 : b.h = 18 : b.b = 256 : b.text = "osc1 vol: "+Str(i.osc1.vol)           : b.p = i.osc1.vol      : b.m = 256 : but(nsbut.osc1_vol) = b
	b.x = 33*32 : b.y = 29*20 : b.h = 18 : b.b = 128 : b.text = "osc1 oct: "+Str(i.osc1.octv)          : b.p = i.osc1.octv     : b.m = 16  : but(nsbut.osc1_oct) = b
	b.x = 38*32 : b.y = 29*20 : b.h = 18 : b.b =  96 : b.text = "osc1 det: "+Str(i.osc1.det)           : b.p = i.osc1.det      : b.m = 12  : but(nsbut.osc1_det) = b
	b.x = 33*32 : b.y = 28*20 : b.h = 18 : b.b = 256 : b.text = "osc1 detune: "+Str(i.osc1.detune)     : b.p = i.osc1.detune   : b.m = 256 : but(nsbut.osc1_detune) = b
	b.x = 33*32 : b.y = 27*20 : b.h = 18 : b.b =  96 : b.text = "osc1 xenv: "+Str(i.osc1.xenv)         : b.p = i.osc1.xenv     : b.m = 2   : but(nsbut.osc1_xenv) = b
	b.x = 37*32 : b.y = 27*20 : b.h = 18 : b.b = 128 : b.text = "osc1 waveform: "+Str(i.osc1.waveform) : b.p = i.osc1.waveform : b.m = 4   : but(nsbut.osc1_waveform) = b

	b.x = 33*32 : b.y = 25*20 : b.h = 18 : b.b = 256 : b.text = "osc2 vol: "+Str(i.osc2.vol)           : b.p = i.osc2.vol      : b.m = 256 : but(nsbut.osc2_vol) = b
	b.x = 33*32 : b.y = 24*20 : b.h = 18 : b.b = 128 : b.text = "osc2 oct: "+Str(i.osc2.octv)          : b.p = i.osc2.octv     : b.m = 16  : but(nsbut.osc2_oct) = b
	b.x = 38*32 : b.y = 24*20 : b.h = 18 : b.b =  96 : b.text = "osc2 det: "+Str(i.osc2.det)           : b.p = i.osc2.det      : b.m = 12  : but(nsbut.osc2_det) = b
	b.x = 33*32 : b.y = 23*20 : b.h = 18 : b.b = 256 : b.text = "osc2 detune: "+Str(i.osc2.detune)     : b.p = i.osc2.detune   : b.m = 256 : but(nsbut.osc2_detune) = b
	b.x = 33*32 : b.y = 22*20 : b.h = 18 : b.b =  96 : b.text = "osc2 xenv: "+Str(i.osc2.xenv)         : b.p = i.osc2.xenv     : b.m = 2   : but(nsbut.osc2_xenv) = b
	b.x = 37*32 : b.y = 22*20 : b.h = 18 : b.b = 128 : b.text = "osc2 waveform: "+Str(i.osc2.waveform) : b.p = i.osc2.waveform : b.m = 4   : but(nsbut.osc2_waveform) = b

	b.x = 33*32 : b.y = 20*20 : b.h = 18 : b.b = 256 : b.text = "noise vol: "+Str(i.noise.fader) : b.p = i.noise.fader : b.m = 256 : but(nsbut.noise_fader) = b

	b.x = 33*32 : b.y = 18*20 : b.h = 18 : b.b = 256 : b.text = "env vol: "+Str(i.env.master)      : b.p = i.env.master  : b.m = 256    : but(nsbut.env_master) = b
	b.x = 33*32 : b.y = 17*20 : b.h = 18 : b.b = 256 : b.text = "env attack: "+Str(i.env.attack)   : b.p = i.env.attack  : b.m = 100001 : but(nsbut.env_attack) = b
	b.x = 33*32 : b.y = 16*20 : b.h = 18 : b.b = 256 : b.text = "env sustain: "+Str(i.env.sustain) : b.p = i.env.sustain : b.m = 100001 : but(nsbut.env_sustain) = b
	b.x = 33*32 : b.y = 15*20 : b.h = 18 : b.b = 256 : b.text = "env release: "+Str(i.env.release) : b.p = i.env.release : b.m = 100001 : but(nsbut.env_release) = b
	b.x = 33*32 : b.y = 14*20 : b.h = 18 : b.b =  96 : b.text = "env hold: "+Str(i.env.hold)       : b.p = i.env.hold    : b.m = 2      : but(nsbut.env_hold) = b

	b.x = 33*32 : b.y = 12*20 : b.h = 18 : b.b = 256 : b.text = "lfo amt: "+Str(i.lfo.amt)             : b.p = i.lfo.amt       : b.m = 256 : but(nsbut.lfo_amt) = b
	b.x = 33*32 : b.y = 11*20 : b.h = 18 : b.b =  96 : b.text = "lfo freq: "+Str(i.lfo.freq)           : b.p = i.lfo.freq      : b.m = 8   : but(nsbut.lfo_freq) = b
	b.x = 37*32 : b.y = 11*20 : b.h = 18 : b.b = 128 : b.text = "lfo waveform: "+Str(i.lfo.waveform)   : b.p = i.lfo.waveform  : b.m = 4   : but(nsbut.lfo_waveform) = b
	b.x = 33*32 : b.y = 10*20 : b.h = 18 : b.b =  96 : b.text = "lfo fx freq: "+Str(i.lfo.fx_freq)     : b.p = i.lfo.fx_freq   : b.m = 2   : but(nsbut.lfo_fx_freq) = b
	b.x = 37*32 : b.y = 10*20 : b.h = 18 : b.b = 112 : b.text = "lfo osc1 freq: "+Str(i.lfo.osc1_freq) : b.p = i.lfo.osc1_freq : b.m = 2   : but(nsbut.lfo_osc1_freq) = b

	b.x = 33*32 : b.y = 8*20 : b.h = 18 : b.b = 128 : b.text = "fx filter: "+Str(i.fx.filter)         : b.p = i.fx.filter     : b.m = 5     : but(nsbut.fx_filter) = b
	b.x = 33*32 : b.y = 7*20 : b.h = 18 : b.b = 256 : b.text = "fx freq: "+Str(i.fx.freq)             : b.p = i.fx.freq       : b.m = SPS/4 : but(nsbut.fx_freq) = b
	b.x = 33*32 : b.y = 6*20 : b.h = 18 : b.b = 256 : b.text = "fx resonance: "+Str(i.fx.resonance)   : b.p = i.fx.resonance  : b.m = 256   : but(nsbut.fx_resonance) = b
	b.x = 33*32 : b.y = 5*20 : b.h = 18 : b.b = 256 : b.text = "fx delay time: "+Str(i.fx.delay_time) : b.p = i.fx.delay_time : b.m = 9     : but(nsbut.fx_delay_time) = b
	b.x = 33*32 : b.y = 4*20 : b.h = 18 : b.b = 256 : b.text = "fx delay amt: "+Str(i.fx.delay_amt)   : b.p = i.fx.delay_amt  : b.m = 256   : but(nsbut.fx_delay_amt) = b
	b.x = 33*32 : b.y = 3*20 : b.h = 18 : b.b = 256 : b.text = "fx pan freq: "+Str(i.fx.pan_freq)     : b.p = i.fx.pan_freq   : b.m = 8     : but(nsbut.fx_pan_freq) = b
	b.x = 33*32 : b.y = 2*20 : b.h = 18 : b.b = 256 : b.text = "fx pan amt: "+Str(i.fx.pan_amt)       : b.p = i.fx.pan_amt    : b.m = 256   : but(nsbut.fx_pan_amt) = b

	For t = 1 To NBUTTONS
		but(t).show = 1
		If instpanelshow=0 Then
			If t>=nsbut.inst_num And t<=nsbut.fx_pan_amt Then but(t).show = 0
		EndIf
	Next
End Sub


' draw buttons
'
Sub drawbuttons ()
	Dim As Integer t, i
	Dim As Double w, d

	i = panelinstnum

	For t = 1 To NBUTTONS
		If but(t).show Then
			glColor3d (0.9, 0.7, 0.5)
			glBegin (GL_QUADS)
			glVertex3d (but(t).x         , but(t).y         , 0)
			glVertex3d (but(t).x+but(t).b, but(t).y         , 0)
			glVertex3d (but(t).x+but(t).b, but(t).y+but(t).h, 0)
			glVertex3d (but(t).x         , but(t).y+but(t).h, 0)
			glEnd ()

			If but(t).p>=0 Then
				d = but(t).b/but(t).m
				w = d
				If w<2 Then w=2

				glColor3d (1, 0.8, 0.6)
				glBegin (GL_QUADS)
				glVertex3d (but(t).x + (but(t).p+0.5)*d-w*0.5, but(t).y         , 0.1)
				glVertex3d (but(t).x + (but(t).p+0.5)*d+w*0.5, but(t).y         , 0.1)
				glVertex3d (but(t).x + (but(t).p+0.5)*d+w*0.5, but(t).y+but(t).h, 0.1)
				glVertex3d (but(t).x + (but(t).p+0.5)*d-w*0.5, but(t).y+but(t).h, 0.1)
				glEnd ()
			EndIf

			glColor3d (1, 1, 1)
			If t=nsbut.kinst1 Then glColor3d (farben(kinst1).r, farben(kinst1).g, farben(kinst1).b)
			If t=nsbut.kinst2 Then glColor3d (farben(kinst2).r, farben(kinst2).g, farben(kinst2).b)
			If t=nsbut.inst_num Then glColor3d (farben(i).r, farben(i).g, farben(i).b)
			mytextout (but(t).x+8, but(t).y+but(t).h/2-4, 0.2, but(t).text)
		EndIf
	Next
End Sub


' is mouse in a button
'
Function getbutton (x As Integer, y As Integer) As Integer
	Dim As Integer t, b

	b = 0
	For t = 1 To NBUTTONS
		If but(t).show Then
			If x>=but(t).x And y>=but(t).y And x<but(t).x+but(t).b And y<but(t).y+but(t).h Then
				b = t
			EndIf
		EndIf
	Next

	Return b
End Function


' show an fft of the buffer
'
Sub showfft (p As Integer)
	Dim As Integer t, i
	Dim As Double w

	For t = 0 To NX-1
		fftr(t,AA) = 0
		For i = 0 To 7
			fftr(t,AA) += buffer((p+t*8+i)*CH Mod BSIZE) * (t*8+i)/NX/8
		Next
		fftr(t,AA) /= 8
	Next
	fft1d (AA, AF, -1)

	glColor3d (1,1,1)
	glBegin (GL_LINES)
	For t = 0 To NX/2/4
		glVertex3d (t, SY/2-1, 0)
		w = 0
		For i = 0 To 3
			w += Sqr (fftc(t*4+i,AF).r^2 + fftc(t*4+i,AF).i^2)/CLIPVALUE
		Next
		w /= 4
		glVertex3d (t, SY/2 + 50*Log(1+w*20), 0)
	Next
	glEnd ()
End Sub


' initialize voices for being silent and free
'
Sub initvoices ()
	Dim As Integer v

	For v = 1 To NVOICES
		voi(v).free = 1
		voi(v).press = 0
		voi(v).inst = 0
		voi(v).note = 0
		voi(v).c = 0
		voi(v).c1 = 0
		voi(v).c2 = 0
		voi(v).low = 0
		voi(v).band = 0
		voi(v).high = 0
		voi(v).attack = 0
		voi(v).sustain = 0
		voi(v).release = 0

		sch(v).press = 0
		sch(v).inst = 0
		sch(v).note = 0
		sch(v).ee = 0
	Next
End Sub


' make schedule out of song/key upto waveout pointer plus latency
'
Sub doschedule ()
	Dim As Integer v, gef, t, d, i, j, y
	Dim As Double vl, vr

	' get timing information, display in samples
	pl = pn
	pn = getwaveoutposition ()
	pg = pn-pl


	' playing longer than the old end?
	If play=1 Then
		If playcnt>endsong Then endsong = playcnt
	EndIf


	' loop mode
	'If playcnt>selectx+selectb Then playcnt = selectx


	' make schedule out of songevents/keystrokes

	plag = pn+LAT-vp

	While vp<pn+LAT

		' play the events of the playsong
		While play=1 And playcnt>=songplay(ep).t
			If songplay(ep).press=1 Then
				v = freevoice ()
				If v>0 Then
					voi(v).free = 0
					sch(v).press = 1
					sch(v).inst = songplay(ep).inst
					sch(v).note = songplay(ep).note
					sch(v).ee = ep
				EndIf
			EndIf

			If songplay(ep).press=0 Then
				For v = 1 To NVOICES
					If sch(v).ee = songplay(ep).ee Then
						sch(v).press = 0
						sch(v).ee = 0
					EndIf
				Next
			EndIf

			ep += 1
		Wend


		' key press/release

		For t = 0 To NKEYS-1
			key(t).last = key(t).act
			If t<NKBKEYS Then
				If GetAsyncKeyState(keys(t)) Then key(t).act = 1 : Else key(t).act = 0
			Else
				If midikey(t-NKBKEYS+36) Then key(t).act = 1 : Else key(t).act = 0
			EndIf
		Next


		For t = 0 To NKEYS-1

			'key pressed
			If key(t).act=1 And key(t).last=0 Then

				If play=1 Then
					key(t).ee = er
					'songrec(er).t = Int(playcnt/rowlen+0.5)*rowlen
					songrec(er).t = playcnt
					songrec(er).inst = key(t).inst
					songrec(er).note = key(t).note
					songrec(er).press = 1
					songrec(er).ee = 0
					er += 1
				EndIf

				v = freevoice ()
				key(t).voi = v
				If v>0 Then
					voi(v).free = 0

					sch(v).press = 1
					sch(v).inst = key(t).inst
					sch(v).note = key(t).note
				EndIf

				down+=1
			EndIf

			' key released
			If key(t).act=0 And key(t).last=1 Then

				If play=1 Then
					'songrec(er).t = Int(playcnt/rowlen+0.5)*rowlen-100
					songrec(er).t = playcnt
					songrec(er).inst = key(t).inst
					songrec(er).note = key(t).note
					songrec(er).press = 0
					d = key(t).ee
					songrec(er).ee = d
					songrec(d).ee = er
					'If songrec(d).t>songrec(er).t Then songrec(er).t+=rowlen
					er += 1
				EndIf

				v = key(t).voi
				If v>0 Then
					sch(v).press = 0
				EndIf

				up+=1
			EndIf
		Next

		For v = 1 To NVOICES
			schbuf(v,vp Mod SCHSIZE) = sch(v)
		Next

		vp += 1
	Wend
End Sub


' sonant synth
'
Dim Shared As UInteger sonantrandseed = 1

Function sonantrand () As Double
	sonantrandseed *= &h15a4e35
	Return (sonantrandseed Mod 255) / 255
End Function


Function getnotefreq (n As Integer) As Double
	'Return 2^((n-128)/12)/256		' = 868.16435/SPS*2^((n-156)/12)
	Return 880/SPS*2^((n-156)/12)
End Function


Function getoscoutput (n As Integer, f As Double) As Double
	If n=0 Then
		Return Sin(PI*f)
	ElseIf n=1 Then
		If Sin(PI*f)<0 Then Return -1 : Else Return 1
	ElseIf n=2 Then
		Return Frac(f) - 0.5
	ElseIf n=3 Then
		f = Frac(f)*4
		If f<2 Then Return f-1 : Else Return 3-f
	EndIf
End Function


'Dim Shared As Double testbuf(SX)

' make one sample for a voice
'
Sub dovoice (v As Integer, p As Integer, t As Integer)
	Dim As Integer attack, sustain, release
	Dim As Integer i, n, c, j
	Dim As Double lfor, c1, c2, f, e, r, sample, vl, vr

	i = voi(v).inst : If i=0 Then Return
	n = voi(v).note

	' LFO
	lfor = getoscoutput (inst(i).lfo.waveform, voi(v).lfof*p/rowlen) * inst(i).lfo.amt/256*0.5 + 0.5

	' envelope
	c = voi(v).c
	attack = voi(v).attack
	sustain = voi(v).sustain
	release = voi(v).release
	If c<attack Then
		e = c/attack
	ElseIf c<attack+sustain Then
		e = 1
		If inst(i).env.hold And voi(v).press=1 Then voi(v).sustain+=1
	ElseIf c<attack+sustain+release Then
		e = 1-(c-attack-sustain)/release
	Else
		e = 0
		voi(v).free = 1
		voi(v).inst = 0
	EndIf

	' oscillator 1
	f = voi(v).osc1f
	If inst(i).lfo.osc1_freq Then f += lfor
	If inst(i).osc1.xenv Then f *= e^2
	voi(v).c1 += f
	r = getoscoutput (inst(i).osc1.waveform, voi(v).c1)
	sample = r*inst(i).osc1.vol/255

	' oscillator 2
	f = voi(v).osc2f
	'If inst(i).lfo.osc2_freq Then f += lfor
	If inst(i).osc2.xenv Then f *= e^2
	voi(v).c2 += f
	r = getoscoutput (inst(i).osc2.waveform, voi(v).c2)
	sample += r*inst(i).osc2.vol/255

	' noise
	sample += Sin(PI*sonantrand ())*inst(i).noise.fader/255*e

	' state variable filter
	f = inst(i).fx.freq
	If inst(i).lfo.fx_freq Then f *= lfor
	f = 1.5*Sin(PI*f/2/SPS)
	voi(v).low += f*voi(v).band
	voi(v).high = voi(v).q*(sample-voi(v).band) - voi(v).low
	voi(v).band += f*voi(v).high
	Select Case inst(i).fx.filter
		Case 1 : sample = voi(v).high
		Case 2 : sample = voi(v).low
		Case 3 : sample = voi(v).band
		Case 4 : sample = voi(v).low + voi(v).high
	End Select

	' envelope
	sample *= e

	' pan
	f = Sin(PI*voi(v).panf*p/rowlen) * inst(i).fx.pan_amt/256*0.5 + 0.5
	sample *= inst(i).env.master/255 * (255*156)	* OUTPUTVOLUME ' 255*156=39780
	vl = sample*(1-f)
	vr = sample*f

	' add to instrument buffer
	thr(t).dbufl(i,thr(t).dbwp Mod DBSIZE) += vl
	thr(t).dbufr(i,thr(t).dbwp Mod DBSIZE) += vr

	'If c/50<SX Then
	'	testbuf(c/50) = vl/32000*200
	'	'testbuf(c/50) = voi(v).low*200
	'	'testbuf(c/50) = voi(v).band*200
	'	'testbuf(c/50) = voi(v).high*200
	'EndIf

	' that's one sample
	voi(v).c += 1
End Sub


' convert schedule to data in voice's delay buffer
'
Sub doscheduletovoice (t As Integer)
	Dim As Integer i, n, v, j, y
	Dim As Integer dtim
	Dim As Double damt

	If t=0 Then vlag = vp-thr(t).dbwp

	While thr(t).dbwp<vp

		For v = t*NVPERT+1 To t*NVPERT+NVPERT
			If schbuf(v,thr(t).dbwp Mod SCHSIZE).press=1 And voi(v).press=0 Then
				i = schbuf(v,thr(t).dbwp Mod SCHSIZE).inst
				n = schbuf(v,thr(t).dbwp Mod SCHSIZE).note

				voi(v).press = 1
				voi(v).inst = i
				voi(v).note = n
				voi(v).c1 = 0
				voi(v).c2 = 0
				voi(v).c = 0
				voi(v).low = 0
				voi(v).high = 0
				voi(v).band = 0

				voi(v).attack = inst(i).env.attack
				voi(v).sustain = inst(i).env.sustain
				voi(v).release = inst(i).env.release
				voi(v).osc1f = getnotefreq (n + (inst(i).osc1.octv-8)*12 + inst(i).osc1.det)
				voi(v).osc1f *= 1 + 0.2*inst(i).osc1.detune/255
				voi(v).osc2f = getnotefreq (n + (inst(i).osc2.octv-8)*12 + inst(i).osc2.det)
				voi(v).osc2f *= 1 + 0.2*inst(i).osc2.detune/255
				voi(v).lfof = 0.5^(8-inst(i).lfo.freq)
				voi(v).panf = 0.5^(8-inst(i).fx.pan_freq)
				voi(v).q = inst(i).fx.resonance/255
			EndIf

			If schbuf(v,thr(t).dbwp Mod SCHSIZE).press=0 And voi(v).press=1 Then
				voi(v).press = 0
			EndIf
		Next

		If play=1 Then
			For v = t*NVPERT+1 To t*NVPERT+NVPERT
				dovoice (v, thr(t).playcnt, t)
			Next
		Else
			For v = t*NVPERT+1 To t*NVPERT+NVPERT
				dovoice (v, thr(t).dbwp, t)
			Next
		EndIf

		For i = 1 To ninst
			' delay
			dtim = inst(i).fx.delay_time * rowlen/2
			If dtim>=DBSIZE Then End
			dtim = thr(t).dbwp - dtim
			While dtim<0 : dtim+=DBSIZE : Wend
			damt = inst(i).fx.delay_amt/255

			thr(t).dbufl(i,thr(t).dbwp Mod DBSIZE) += thr(t).dbufr(i,dtim Mod DBSIZE)*damt
			thr(t).dbufr(i,thr(t).dbwp Mod DBSIZE) += thr(t).dbufl(i,dtim Mod DBSIZE)*damt

			thr(t).dbufl(i,(thr(t).dbwp+1) Mod DBSIZE) = 0
			thr(t).dbufr(i,(thr(t).dbwp+1) Mod DBSIZE) = 0
		Next

		thr(t).dbwp += 1
		If play=1 Then thr(t).playcnt += 1
	Wend

	thr(t).pt = thr(t).dbwp
End Sub


' clip sample values to short range
'
Function clip (x As Integer) As Integer
	'Return (2/(1+Exp(-2*x/CLIPVALUE))-1)*CLIPVALUE
	If x> CLIPVALUE Then Return  CLIPVALUE
	If x<-CLIPVALUE Then Return -CLIPVALUE
	Return x
End Function


' read from voice buffers, add, write to waveout buffer
'
Sub dowaveout ()
	Dim As Integer gef, t, j, i
	Dim As Double vl, vr

	' add samples from threads, write to waveout buffer
	gef = thr(0).pt
	For t = 1 To NTHR-1
		If thr(t).pt<gef Then gef = thr(t).pt
	Next t

	wlag = gef-pt

	While pt<gef
		j = pt*CH Mod BSIZE
		buffer(j  ) = 0
		buffer(j+1) = 0
		For i = 1 To ninst
			vl = 0
			vr = 0
			For t = 0 To NTHR-1
				vl += thr(t).dbufl(i,pt Mod DBSIZE)
				vr += thr(t).dbufr(i,pt Mod DBSIZE)
			Next

			buffer(j  ) = clip (buffer(j  ) + Int(vl))
			buffer(j+1) = clip (buffer(j+1) + Int(vr))
		Next

		If WRITEWAV And play=1 And playcnt*2<recbuflen Then
			*(recbuf + playcnt*2  ) = buffer(j  )
			*(recbuf + playcnt*2+1) = buffer(j+1)
		EndIf

		pt += 1
		If play=1 Then playcnt += 1
	Wend
End Sub


Sub thrprocschedule (param As Any Ptr)
	Dim As Integer t

	Do
		WaitForSingleObject (htmr, INFINITE)
		doschedule ()
		For t = 0 To NTHR-1
			PulseEvent (hevtsch(t))
		Next
	Loop
End Sub


Sub thrproc (param As Any Ptr)
	Dim As Integer Ptr para
	Dim As Integer t

	para = Cast (Integer Ptr, param)
	t = *para
	thr(t).pt = *(para+1)
	Do
		WaitForSingleObject (hevtsch(t), INFINITE)
		doscheduletovoice (t)
		SetEvent (hevtvoi(t))
	Loop
End Sub


Sub thrprocwaveout (param As Any Ptr)
	Dim As Integer t

	Do
		WaitForMultipleObjects (NTHR, @hevtvoi(0), 1, INFINITE)
		For t = 0 To NTHR-1
			ResetEvent (hevtvoi(t))
		Next
		dowaveout ()
	Loop
End Sub


' do graphics (draw song, buttons, text, etc)
'
Sub dographics ()
	Dim As Integer t, n
	Dim As String s
	Dim As Integer minutes, seconds, hundred, samples

	glClear (GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)

	drawsongs ()

	glColor3d (1,1,1)
	mytextout (20, SY-1*20, 0, Str(pg))
	'mytextout (20, SY-3*20, 0, Str(ep-1))
	'mytextout (20, SY-4*20, 0, Str(er-1))
	'mytextout (20, SY-6*20, 0, Str(plag))
	'mytextout (20, SY-7*20, 0, Str(vlag))
	'mytextout (20, SY-8*20, 0, Str(wlag))

	' show key up/down counters
	'mytextout (20, SY-70, 0, Str(up)+"  "+Str(down))

	' update playcnt button
	minutes = Int(playcnt/SPS/60)
	seconds = Int(playcnt/SPS) Mod 60
	hundred = Int(playcnt/(SPS/100)) Mod 100
	samples = playcnt Mod (SPS/100)

	s = Str(minutes)+":"+Format(seconds,"00")+"."+Format(hundred,"00")+","+Format(samples,"000")
	but(nsbut.playcnt).text = s
	but(nsbut.playcnt).p = playcnt
	but(nsbut.playcnt).m = endsong

	drawbuttons ()


	' show thread pointers
	'For t = 0 To NTHR-1
	'	mytextout (20, 200+20*t, 0, Str(thr(t).pt))
	'Next


	' show fft
	If FFTSWITCH Then showfft (pt)


	' show testbuffer
	'glColor3d (1,1,1)
	'glBegin (GL_LINES)
	'For t=0 To SX-1
	'	glVertex3d (t, SY/2, 0)
	'	glVertex3d (t, SY/2+testbuf(t), 0)
	'Next
	'glEnd


	' show voice occupation

	's = Space((NVOICES+1)*2)
	'For n = 1 To NVOICES
	'	Mid(s,n*2,1) = Str(isfree(n))
	'Next
	'mytextout (20, 100, 0, s)

	's = Space((NVOICES+1)*2)
	'For n = 1 To NVOICES
	'	Mid(s,n*2,1) = Str(voi(n).free)
	'Next
	'mytextout (20, 120, 0, s)

	's = Space((NVOICES+1)*2)
	'For n = 1 To NVOICES
	'	Mid(s,n*2,1) = Str(voi(n).press)
	'Next
	'mytextout (20, 140, 0, s)


	' show virtual key codes
	'n = SY/2
	'For t = 0 To 255
	'	If GetAsyncKeyState(t) Then
	'		mytextout (SX/2, n, 0, Str(t))
	'		n += 20
	'	EndIf
	'Next
End Sub


' sets new value and text of button according to slider pos
'
Sub newbuttonvalue (b As Integer)
	Dim As Integer i, n

	i = panelinstnum
	n = Int(but(b).p)
	Select Case b
		Case nsbut.playcnt : If play=0 Then playcnt = n

		Case nsbut.bpm
			bpm = n
			If bpm<60 Then bpm=60
			rowlen = Int(SPS/bpm/4*60+0.5)

		Case nsbut.koct1  : koct1 = n    : initkeys ()
		Case nsbut.kinst1 : kinst1 = n+1 : initkeys ()
		Case nsbut.koct2  : koct2 = n    : initkeys ()
		Case nsbut.kinst2 : kinst2 = n+1 : initkeys ()

		Case nsbut.inst_num  : panelinstnum = n+1
		Case nsbut.inst_name : inst(panelinstnum) = instbank(n+1)

		Case nsbut.osc1_vol      : inst(i).osc1.vol = n
		Case nsbut.osc1_oct      : inst(i).osc1.octv = n
		Case nsbut.osc1_det      : inst(i).osc1.det = n
		Case nsbut.osc1_detune   : inst(i).osc1.detune = n
		Case nsbut.osc1_waveform : inst(i).osc1.waveform = n
		Case nsbut.osc1_xenv     : inst(i).osc1.xenv = n

		Case nsbut.osc2_vol      : inst(i).osc2.vol = n
		Case nsbut.osc2_oct      : inst(i).osc2.octv = n
		Case nsbut.osc2_det      : inst(i).osc2.det = n
		Case nsbut.osc2_detune   : inst(i).osc2.detune = n
		Case nsbut.osc2_waveform : inst(i).osc2.waveform = n
		Case nsbut.osc2_xenv     : inst(i).osc2.xenv = n

		Case nsbut.noise_fader : inst(i).noise.fader = n

		Case nsbut.env_master  : inst(i).env.master = n
		Case nsbut.env_attack  : inst(i).env.attack = n
		Case nsbut.env_sustain : inst(i).env.sustain = n
		Case nsbut.env_release : inst(i).env.release = n
		Case nsbut.env_hold    : inst(i).env.hold = n

		Case nsbut.lfo_freq      : inst(i).lfo.freq = n
		Case nsbut.lfo_fx_freq   : inst(i).lfo.fx_freq = n
		Case nsbut.lfo_osc1_freq : inst(i).lfo.osc1_freq = n
		Case nsbut.lfo_amt       : inst(i).lfo.amt = n
		Case nsbut.lfo_waveform  : inst(i).lfo.waveform = n

		Case nsbut.fx_filter     : inst(i).fx.filter = n
		Case nsbut.fx_freq       : inst(i).fx.freq = n
		Case nsbut.fx_resonance  : inst(i).fx.resonance = n
		Case nsbut.fx_delay_time : inst(i).fx.delay_time = n
		Case nsbut.fx_delay_amt  : inst(i).fx.delay_amt = n
		Case nsbut.fx_pan_freq   : inst(i).fx.pan_freq = n
		Case nsbut.fx_pan_amt    : inst(i).fx.pan_amt = n
	End Select
End Sub


' open load dialog and load song
'
Sub loadsong_dialog ()
	Dim As OPENFILENAME ofn
	Dim As String szFile, fil, cur

	'ShowWindow (hwnd, SW_MINIMIZE)

	fil = "songfiles (.txt)"+Chr(0)+"*.txt"+Chr(0)
	cur = CurDir
	ZeroMemory (@ofn, SizeOf(ofn))
	ofn.lStructSize = SizeOf(ofn)
	ofn.hwndOwner = hwnd
	szFile = Space(256)
	Mid (szFile,1,1) = Chr(0)
	ofn.lpstrFile = StrPtr(szFile)
	ofn.nMaxFile = Len(szFile)
	ofn.lpstrFilter = StrPtr(fil)
	ofn.nFilterIndex = 1
	ofn.lpstrFileTitle = NULL
	ofn.nMaxFileTitle = 0
	ofn.lpstrInitialDir = StrPtr(cur)
	ofn.Flags = OFN_PATHMUSTEXIST Or OFN_FILEMUSTEXIST

	If GetOpenFileName (@ofn) Then
		loadsong (szFile)
		endsong = lengthsong ()
	EndIf

	'ShowWindow (hwnd, SW_MAXIMIZE)
End Sub


' open save dialog and save song
'
Sub savesong_dialog ()
	Dim As OPENFILENAME ofn
	Dim As String szFile, fil, cur

	'ShowWindow (hwnd, SW_MINIMIZE)

	fil = "songfiles (.txt)"+Chr(0)+"*.txt"+Chr(0)
	cur = CurDir
	ZeroMemory (@ofn, SizeOf(ofn))
	ofn.lStructSize = SizeOf(ofn)
	ofn.hwndOwner = hwnd
	szFile = Space(256)
	Mid (szFile,1,1) = Chr(0)
	ofn.lpstrFile = StrPtr(szFile)
	ofn.nMaxFile = Len(szFile)
	ofn.lpstrFilter = StrPtr(fil)
	ofn.nFilterIndex = 1
	ofn.lpstrFileTitle = NULL
	ofn.nMaxFileTitle = 0
	ofn.lpstrInitialDir = StrPtr(cur)
	ofn.Flags = OFN_PATHMUSTEXIST Or OFN_FILEMUSTEXIST

	If GetSaveFileName (@ofn) Then
		savesong (szFile)
	EndIf

	'ShowWindow (hwnd, SW_MAXIMIZE)
End Sub


' mouse position and clicking
'
Sub domousestuff ()
	Dim As Integer x, y, b, t
	Dim As Integer buttons, mouseclip
	Dim As Double dv

	If GetAsyncKeyState(VK_SHIFT) Then shift = 1 : Else shift = 0
	If GetAsyncKeyState(VK_CONTROL) Then control = 1 : Else control = 0
	If GetAsyncKeyState(VK_MENU) Then altkey = 1 : Else altkey = 0
	If GetAsyncKeyState(VK_TAB) Then tabkey = 1 : Else tabkey = 0

	If selectbox Then
		If GetAsyncKeyState(VK_DELETE) Then selectiondelete (0) : selectbox=0
		If GetAsyncKeyState(VK_BACK) Then selectiondelete (selectb) : selectbox=0
	EndIf

	If GetAsyncKeyState(VK_PRIOR) Then insdelspace (100)
	If GetAsyncKeyState(VK_NEXT) Then insdelspace (-100)


	' test other keys
	For t = 0 To NOTHKEYS-1
		othkey(t).last = othkey(t).act
		If GetAsyncKeyState(othkeys(t)) Then othkey(t).act = 1 : Else othkey(t).act = 0
	Next

	' instrument panel show/hide key
	If othkey(nsothkey.showinst).act=1 And othkey(nsothkey.showinst).last=0 Then
		instpanelshow = 1-instpanelshow
		initbuttons ()
	EndIf

	' instrument save to bank key
	If othkey(nsothkey.saveinst).act=1 And othkey(nsothkey.saveinst).last=0 Then
		If instpanelshow And ninstbank<NINSTBANKMAX Then
			t = panelinstnum
			inst(t).nam = inst(t).nam+"_mod"+Str(modcnt)
			modcnt += 1
			ninstbank += 1
			inst(t).banknum = ninstbank
			instbank(ninstbank) = inst(t)
			initbuttons ()
		EndIf
	EndIf


	' get mouse position and buttons
	oldwheel = wheel
	GetMouse x, y, wheel, buttons, mouseclip
	If x=-1 And y=-1 And wheel=-1 And buttons=-1 Then wheel = oldwheel : Exit Sub
	'If mouseclip=1 Then wheel = oldwheel : Exit Sub
	
	If buttons And 1 Then
		If holdl=0 Then firstl = 1 : Else firstl = 0
		holdl = 1 : lastl = 0
	Else
		If holdl=1 Then lastl = 1 : Else lastl = 0
		holdl = 0 : firstl = 0
	EndIf
	If buttons And 2 Then
		If holdr=0 Then firstr = 1 : Else firstr = 0
		holdr = 1
	Else
		holdr = 0 : firstr = 0
	EndIf

	y = SY-y-1	' convert GetMouse coos to OpenGL coos

	b = getbutton (x,y)

	' mouse wheel
	If wheel<>oldwheel Then
		If b=0 Then
			zoom *= 1.1^(oldwheel-wheel)
		Else
			If b=nsbut.playcnt Then			' playcnt button increments
				If control=1 Then
					dv = 1*rowlen
				ElseIf shift=1 Then
					dv = 8*rowlen
				ElseIf altkey=1 Then
					dv = 32*rowlen
				ElseIf tabkey=1 Then
					dv = 256*rowlen
				Else
					dv = but(b).m/32
					If dv<1 Then dv = 1
				EndIf
			Else									' other buttons increments
				If control=1 Then
					dv = 1
				ElseIf shift=1 Then
					dv = 10
				ElseIf altkey=1 Then
					dv = 100
				ElseIf tabkey=1 Then
					dv = 1000
				Else
					dv = but(b).m/32
					If dv<1 Then dv = 1
				EndIf
			EndIf
			If oldwheel>wheel Then
				but(b).p -= Int(dv*(oldwheel-wheel)+0.5)
			Else
				but(b).p += Int(dv*(wheel-oldwheel)+0.5)
			EndIf
			If but(b).p<0 Then but(b).p=0
			If but(b).p>but(b).m-1 Then but(b).p = but(b).m-1
			newbuttonvalue (b)
			initbuttons ()
		EndIf
	EndIf

	' left click in slider
	If holdl And pangrab=0 And b>0 And but(b).p>=0 Then
		but(b).p = Int((x-but(b).x)/but(b).b*but(b).m)
		newbuttonvalue (b)
		initbuttons ()
		initkeys ()
	EndIf

	' left click in button
	If firstl Then

		If b=nsbut.play Then		' play button

			If play=1 Then								' stop
				songrec(er).t = TEND
				ep = 1
				clearvoices ()
				initvoices ()
				play = 0
				but(nsbut.play).text = "play"
			Else											' play
				er = 1
				songrec(er).t = TEND
				ep = 1
				While songplay(ep).t<playcnt : ep+=1 : Wend
				' if play=0 it is safe to write to thr(t).playcnt
				For t = 0 To NTHR-1
					thr(t).playcnt = playcnt
				Next
				play = 1
				but(nsbut.play).text = "stop"
				pcstart = getwaveoutposition() - playcnt
				'pcstart = Int(pcdouble) - playcnt
			EndIf

		ElseIf b=nsbut.load Then		' load

			'loadsong (LOADSONGNAME)
			'endsong = lengthsong ()
			loadsong_dialog ()
			initbuttons ()

		ElseIf b=nsbut.save Then		' save

			'savesong (SAVESONGNAME)
			savesong_dialog ()

		ElseIf b=nsbut.clr Then		' clear song

			ep = 1
			songplay(ep).t = TEND
			endsong = lengthsong ()

		ElseIf b=nsbut.merge Then		' merge

			merge ()
			er = 1
			songrec(er).t = TEND

		EndIf

	EndIf


	' left mousebutton pressed for panning

	If firstl And control=0 And b=0 Then		' no button, pan
		If play=0 Then
			mx = x
			mplaycnt = playcnt
			ep = 1
		EndIf
		my = y
		mplaydy = playdy
		pangrab = 1
	EndIf

	If holdl And control=0 And pangrab Then
		If play=0 Then playcnt = mplaycnt - (x-mx)*SPS/100*zoom
		playdy = mplaydy - (y-my)*SPS/100*zoom
	EndIf

	If lastl And control=0 Then
		If play=0 Then clearvoices ()
		pangrab = 0
	EndIf


	' left mouse button for selectbox

	If firstl And control=1 And play=0 And b=0 Then
		mx = x
		my = y
		selectx = playcnt + (mx-SX/2)*SPS/100*zoom
		selecty = playdy + (my-SY/2)*SPS/100*zoom
		selectb = 1
		selecth = 1
		selectbox = 1
	EndIf

	If holdl And control=1 And play=0 And selectbox Then
		selectb = (x-mx)*SPS/100*zoom
		selecth = (y-my)*SPS/100*zoom
	EndIf

	'If lastl And control=1 And play=0 Then
	'	selectbox = 0
	'EndIf
End Sub


' midi in callback function
'
Sub callbackfunc (hMidiIn As HMIDIIN, wMsg As UINT, dwInstance As DWORD_PTR, dwParam1 As DWORD_PTR, dwParam2 As DWORD_PTR)
	Dim As Integer channel, taste, code, vel

	'Print Hex(dwParam1,8);" ";
	'Print Using "##.### ";(Timer-tim)*1000-dwParam2;

	channel = LoByte(LoWord(dwParam1)) And 15
	code = LoByte(LoWord(dwParam1)) Shr 4
	taste = HiByte(LoWord(dwParam1))
	vel = LoByte(HiWord(dwParam1))

	Select Case wMsg
		Case MIM_OPEN
			'Print "open msg ";

		Case MIM_DATA
			If code=9 Then midikey(taste) = 1
			If code=8 Then midikey(taste) = 0
			'Print channel;code;taste;vel

		Case Else
			'Print "unknown msg ";
	End Select
End Sub


' main
'
Sub main ()
	Dim As Integer t, i
	Dim As HDC hdc
	Dim As HGLRC hglrc
	Dim As Integer thrparam(NTHR,2)
	Dim As LARGE_INTEGER li
	Dim As Double tim1, tim2, qfreq
	Dim As LPDIRECTDRAW lpDD
	Dim As Integer monitorfreq
	Dim As Integer result
	Dim As HMIDIIN hmi


	Randomize

	'ScreenRes SX,SY,32,,FB.GFX_OPENGL+FB.GFX_MULTISAMPLE+FB.GFX_FULLSCREEN
	ScreenRes SX,SY,32,,FB.GFX_OPENGL+FB.GFX_MULTISAMPLE

	ScreenControl (FB.GET_WINDOW_HANDLE, Cast (Integer, hwnd))
	hdc = GetDC (hwnd)
	hglrc = wglCreateContext (hdc)
	wglMakeCurrent (hdc, hglrc)
	SelectObject (hdc, GetStockObject (SYSTEM_FONT))
	wglUseFontBitmaps (hdc, 0, 255, 1000)

	' turn vertical sync off (otherwise it is on by default)
	'Dim SwapInterval As Function (ByVal interval As Integer) As Integer
	'SwapInterval = ScreenGLProc ("wglSwapIntervalEXT")
	'SwapInterval (0)

	glViewport (0, 0, SX, SY)
	glMatrixMode (GL_PROJECTION)
	glLoadIdentity ()
	glOrtho (0, SX, 0, SY, -1, 1)
	glMatrixMode (GL_MODELVIEW)
	glLoadIdentity ()

	glClearColor (0.8, 0.6, 0.4, 1)		' background color
	glEnable (GL_DEPTH_TEST)

	If FFTSWITCH Then fft_init ()

	ninst = NINSTMAX

	koct1 = 4
	koct2 = 5
	kinst1 = 1
	kinst2 = 2

	initkeys ()
	initbuttons ()
	initvoices ()

	er = 1
	songrec(er).t = TEND
	ep = 1
	songplay(ep).t = TEND

	wfx.wFormatTag = WAVE_FORMAT_PCM
	wfx.nChannels = CH
	wfx.nSamplesPerSec = SPS
	wfx.nAvgBytesPerSec = SPS*CH*BYTES
	wfx.nBlockAlign = CH*BYTES
	wfx.wBitsPerSample = BITS
	wfx.cbSize = 0

	waveOutOpen (@hWaveOut, WAVE_MAPPER, @wfx, 0, 0, CALLBACK_NULL)

	pt = LAT
	For t = 0 To NTHR-1
		thr(t).dbwp = LAT
	Next
	play = 0
	playcnt = 0
	vp = LAT
	bpm = 120 : rowlen = Int(60*SPS/bpm/4+0.5)
	zoom = 1

	instpanelshow = 1
	panelinstnum = 1

	clearbuffer ()
	startbuffer ()

	progdir = CurDir
	loadinstrumentbank (LOADINSTRUMENTBANKNAME)

	For t = 1 To ninst
		setinstrument (t, "-")
	Next

	'loadsong (LOADSONGNAME)
	ep = 1 : songplay(ep).t = TEND		' only the instruments of the song
	endsong = lengthsong ()


	If WRITEWAV Then
		recbuflen = endsong*2
		recbuf = Callocate (recbuflen, SizeOf(Short))
	EndIf

	initbuttons ()


	DirectDrawCreate (0, @lpDD, 0)

	IDirectDraw_GetMonitorFrequency (lpDD, @monitorfreq)

	QueryPerformanceFrequency (@li) : qfreq = li.LowPart

	timeBeginPeriod (1)		' set 1 ms resolution

	htmr = CreateWaitableTimer (0,0,0)
	li.QuadPart = -10000
	SetWaitableTimer (htmr, @li, 1, 0,0,0)		' 1 ms periodic

	For t = 0 To NTHR-1
		hevtsch(t) = CreateEvent (0,0,0,0)
		hevtvoi(t) = CreateEvent (0,0,0,0)
	Next

	ThreadCreate (@thrprocwaveout, 0)
	For t = 0 To NTHR-1
		thrparam(t,0) = t : thrparam(t,1) = pt
		ThreadCreate (@thrproc, @thrparam(t,0))
	Next
	ThreadCreate (@thrprocschedule, 0)


	result = midiinopen (@hmi, 0, Cast (DWORD_PTR, @callbackfunc), 0, CALLBACK_FUNCTION)
	'If result=MMSYSERR_NOERROR Then Print "ok" : Else Print "error: ";result

	result = midiinstart (hmi)
	'If result=MMSYSERR_NOERROR Then Print "ok" : Else Print "error: ";result




	' main loop
	Do
		glFlush ()
		glFinish ()
		QueryPerformanceCounter (@li) : tim1 = li.QuadPart/qfreq*1000

		domousestuff ()
		dographics ()

		glFlush ()
		glFinish ()
		QueryPerformanceCounter (@li) : tim2 = li.QuadPart/qfreq*1000

		IDirectDraw_WaitForVerticalBlank (lpDD, DDWAITVB_BLOCKEND, 0)
		If play=1 Then
			'pc = playcnt
			pc = getwaveoutposition ()-pcstart
			'pc = Int(pcdouble) - pcstart
		Else
			pc = playcnt
		EndIf
		IDirectDraw_GetScanLine (lpDD, @i)

		glColor3d (1,1,1)
		mytextout (20, SY-2*20, 0, Format(tim2-tim1,"0.00"))
		mytextout (20, SY-3*20, 0, Str(i))
		'mytextout (20, SY-4*20, 0, Str(getwaveoutposition()-Int(pcdouble)))
		'mytextout (20, SY-5*20, 0, Str(monitorfreq))

		If GetAsyncKeyState(VK_ESCAPE) Then Exit Do		' esc

		'pcdouble += SPS/monitorfreq
		Flip
	Loop

	waveOutPause (hWaveOut)
	waveOutUnprepareHeader (hWaveOut, @header, SizeOf(WAVEHDR))
	waveOutClose (hWaveOut)

	ChDir progdir
	saveinstrumentbank (SAVEINSTRUMENTBANKNAME)

	If WRITEWAV Then
		Open "test.wav" For Binary Access write As #1
		Put #1, , *recbuf, recbuflen
		Close #1
		DeAllocate (recbuf)
	EndIf

	Screen 0
End Sub
