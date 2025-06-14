# Frame Chronicle

Just a simple lib I wrote to help me debug some of my game code projects.
The idea is you specify which frames you want to enable recording for, and then
put record statements around your code. Later on you run queries to get it out
and inspect. Example usage:

```lisp
; add "com.thejach.frame-chronicle" to your ASD dependencies or quickload it
; after cloning the repo to somewhere quicklisp can see it.

; in your defpackage, a suggested nickname:
  (:local-nicknames (#:fc #:com.thejach.frame-chronicle))

; somewhere in an init or main, declare you want to collect for the first 10 frames
(fc:make-store :sample-on-frames (loop for i below 10 collect i))

; at the start of a game loop tick:
(fc:frame-tick)

; anywhere else:
(fc:record :obj-new-speed (list vx vy))

; later, typically in the REPL:
(fc:query :obj-new-speed :frames 5)
; --> ((3 4))
; i.e. a list of all values recorded against that key, in this case just one value
```

Other sample criteria include `:sample-always t`, `:sample-every-nth-frame n`,
and `:sample-on-function (lambda (frame) some-condition)`.

You may also call `(fc:clear)` to delete the current data. This also resets the
"frame count", so that the next time `(fc:frame-tick)` is called, recording
state will be back to "frame 0", i.e. the first frame. You could also just make
a new store for the same effect.

Either way, this is another useful pattern:

```lisp
(fc:make-store :sample-on-frames '(0))

; at the start of a game loop tick:
(fc:frame-tick)

; various other places have (fc:record ...) calls, some might be in
; conditionals. Here are some more examples.
; Of course symbols or keywords can be used for the id.

(fc:record 'log "Collision between foo and bar")
(fc:record 'frame-stats (list enemies-destroyed enemies-spawned))
(fc:record 'hit "Normal hit")
(fc:record 'crit "Critical hit")
```

The game completes its game loop, and now no more data will be recorded. At
your leisure in the REPL, you can query information about that frame:

```lisp
(let* ((hits (length (fc:query 'hit :frames 0)))
       (crits (length (fc:query 'crit :frames 0)))
       (total (+ hits crits)))
  (format nil "Total hits: ~a~%Crit rate: ~,2f%" total (* 100 (/ crits total))))
; -->
; "Total hits: 10
; Crit rate: 30.00%"

; get everything:
(fc:query nil)
; -->
; (0 (LOG ("Collision between foo and bar") FRAME-STATS ((20 30)) HIT ("Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit") CRIT ("Critical hit" "Critical hit" "Critical hit")))

; without the leading frame 0 key:
(fc:query nil :frames 0) ; :frames '(0) is also allowed, but will include the (0 ...) prefix as above
; -->
; (LOG ("Collision between foo and bar") FRAME-STATS ((20 30)) HIT ("Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit" "Normal hit") CRIT ("Critical hit" "Critical hit" "Critical hit"))
```

And again, if you want to sample data from a new frame later, all you need to do
is call `(fc:clear)` (or just re-evaluate your `(fc:make-store...)`).

See the test code for even more examples.

# License

This library is licensed under the Unlicense and as such is in the public
domain.
