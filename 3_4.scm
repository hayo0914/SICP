; Concurrency: Time is of the Essence

; The central issue lurking beneath the complexity of state,
; sameness, and change is that by introducing assignment
; we are forced to admit time into our computational
; models.

; Making programs more modular, concurrent computation
; can provide a speed advantage over sequential computation

; Sequential computers execute only one operation at a time,
; so the amount of time it takes to perform a task
; is propotional to the total number of operations performed.

; Howerver, if it is possible to decompose a problem
; into pieces that are relatively independent and
; need to communicate only rarely, it may be possible to 
; allocate pieces to separate computing processors,
; producing a speed advantage proportional to the
; number of processors available.

; Unfortunately, the complexities introduced by 
; assignment become even more problematic in the
; precense of concurrency.

; --------------------
; 3.4.1 The nature of time in concurrent systems.

; The root of this complexity lies in the
; assignments to variables that are shared among
; the different processes.

; One possible restriction on concurrency would
; stipulate that no two operations that change any
; shared state variables can occur at the same time.
; This is an extremely strigent requirement.

; Ex 3.38
; Skip

; --------------------
; 3.4.2 Mechanisms for Controlling Concurrency

; Serializing access to shared state.
; We can use serialization to control access to
; shared variables.

; We ensure that no other procedure that assigns to
; the variable can run concurrently with this procedure
; by serializing all of these procedures with the
; same serializer.

; This guarantees that the value of the variable
; cannot be changed between an access and the
; correnspong assignment.

; --------------------
; Serializers in Scheme



