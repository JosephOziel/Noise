from midiutil import MIDIFile

T = MIDIFile(1)

with open("test.mid", "wb") as file:
    T.writeFile(file)
