# Kill Ring
Emacs has something called the kill ring. This is a circular buffer
where text that is killed is stored. You can then get the text back
out of the kill ring and into the buffer in different ways.

## Builtin kill ring navigation
If you press C-y then the contents of the kill ring are pasted into
the buffer.

If you press M-y then the contents of the kill ring are pasted into
the buffer. If you press M-y again the newly pasted text is replaced
with the next item from the kill ring.

## Helm kill ring navigation
You can use helm to navigate the kill ring. In this case you will get
a list of all the items in the kill ring. You can navigate the list
with the up and down arrows. Pres Ret to select an item and paste it
into the buffer.
