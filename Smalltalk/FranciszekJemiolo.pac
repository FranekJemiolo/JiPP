| package |
package := Package name: 'FranciszekJemiolo'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Punkt;
	add: #Spacer;
	add: #Torus;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Punkt
	instanceVariableNames: 'torus coordinates value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Torus
	instanceVariableNames: 'points shape'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Collection subclass: #Spacer
	instanceVariableNames: 'pointCollection block containsBlock point limit'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Punkt guid: (GUID fromString: '{E304C907-8A03-415C-BC77-ED002060FC9C}')!
Punkt comment: ''!
!Punkt categoriesForClass!Kernel-Objects! !
!Punkt methodsFor!

- j
	|absj coordj newCoords ans|
	"Move one down in given direction inside torus"
	(j = 0) ifTrue: [
		^self].
	absj := j abs.
	coordj := coordinates at: absj.
	newCoords := coordinates copy.
	newCoords at: absj put: coordj -1.
	ans := torus getPointAt: newCoords.
	^ans!

% assoc
	|i j absj sign len col k passed coords pt ans|
	"Get the association key and value, absolute value of direction, and sign of the direction"
	i := assoc key.
	j := assoc value.
	(j = 0) ifTrue:
		[absj := 1.
		sign := 0].
	(j = 0) ifFalse:
		[absj := j abs.
		sign := j / absj].
	
	"Get the length of torus in that dimension"
	len := torus getShape at: absj.
	col := OrderedCollection new.
	"Now insert the first element"
	passed := 1.
	k := coordinates at: absj.
	coords := coordinates copy.
	coords at: absj put: k.
	pt := torus getPointAt: coords.
	col add: pt.
	coords := coordinates copy.
	k := k + sign.

	"Insert new points until we get the size we wanted of the walk."
	[(passed = i)] whileFalse:
		[coords at: absj put: k.
		k := k + sign.
		passed := passed + 1.
		k := k \\ len.
		pt := torus getPointAt: coords.
		col add: pt].
	ans := Spacer return: col.
	^ans
	!

& b
	|col ans|
	col := OrderedCollection new.
	col add: self.
	ans := Spacer return: col withBlock: b andPoint: self.
	^ans
	
	!

@ v
	|k elem coords ans|
	"Move into direction given by vector v"
	k := 1.
	coords := v copy.
	[k <= (v size)] whileTrue:
		[elem := coordinates at: k.
		coords at: k put: (elem + (v at: k)).
		k := k + 1].
	ans := torus getPointAt: coords.
	^ans!

| j
	|absj sign len col k act coords pt ans|
	(j = 0) ifTrue:
		[absj := 1.
		sign := 0].
	(j = 0) ifFalse:
		[absj := j abs.
		sign := j / absj].
	"Get the dimension size in that direction"
	len := torus getShape at: absj.
	col := OrderedCollection new.
	k := coordinates at: absj.
	col add: self.
	"Iterate while there are different points from the first"
	coords := coordinates copy.
	k := k + sign.
	act := k.
	[k = (act - sign)] whileFalse:
		[coords at: absj put: k.
		k := k + sign.
		k := k \\ len.
		pt := torus getPointAt: coords.
		col add: pt].
	ans := Spacer return: col.
	^ans!

+ j
	|absj coordj newCoords ans|
	"Move one up in given direction inside torus"
	(j = 0) ifTrue: [
		^self].
	absj := j abs.
	coordj := coordinates at: absj.
	newCoords := coordinates copy.
	newCoords at: absj put: coordj + 1.
	ans := torus getPointAt: newCoords.
	^ans!

initialize
	value := nil.
	coordinates := nil.
	torus := nil!

printOn: aStream
	^value printOn: aStream!

setCoords: coords setTorus: tor
	coordinates := coords.
	torus := tor!

value
	^value!

value: val
	value := val! !
!Punkt categoriesFor: #-!public! !
!Punkt categoriesFor: #%!public! !
!Punkt categoriesFor: #&!public! !
!Punkt categoriesFor: #@!public! !
!Punkt categoriesFor: #|!public! !
!Punkt categoriesFor: #+!public! !
!Punkt categoriesFor: #initialize!private! !
!Punkt categoriesFor: #printOn:!public! !
!Punkt categoriesFor: #setCoords:setTorus:!public! !
!Punkt categoriesFor: #value!public! !
!Punkt categoriesFor: #value:!public! !

Torus guid: (GUID fromString: '{38B15EDC-B63D-4689-AB65-3F98E9AFB740}')!
Torus comment: ''!
!Torus categoriesForClass!Kernel-Objects! !
!Torus methodsFor!

getPointAt: coord
	"Retrieves the point with given coordinates inside of a torus"
	|k coords elem pt ans|
	coords := coord copy.
	k := 1.
	"Check if the coords are not outside of torus and make mod operation."
	[k <= (coords size)] whileTrue:
		[elem := coords at: k.
		coords at: k put: (elem \\ (self getShape at: k)).
		k := k + 1
		].
	points := self getPoints.
	"Check if the point is already inserted in torus?"
	(points includesKey: coords) ifTrue:
		[ans := points at: coords].
	(points includesKey: coords) ifFalse:
		[pt := Punkt new.
		pt setCoords: coords setTorus: self.
		points at: coords put: pt.
		ans := pt].
		^ans!

getPoints
	^points!

getShape
	^shape
!

initialize
	points := Dictionary new.
	shape := OrderedCollection new.!

insertKey: key withValue: point
	points = self getPoints.
	points at: key put: point!

setPoints: someObject
	points := someObject!

setShape: someObject
	shape := someObject! !
!Torus categoriesFor: #getPointAt:!public! !
!Torus categoriesFor: #getPoints!private! !
!Torus categoriesFor: #getShape!private! !
!Torus categoriesFor: #initialize!private! !
!Torus categoriesFor: #insertKey:withValue:!private! !
!Torus categoriesFor: #setPoints:!private! !
!Torus categoriesFor: #setShape:!private! !

!Torus class methodsFor!

shape: s
	"Creates new torus of shape s, and returns point that belongs to it."
	|torus pt dim coords k pts|
	torus := self new.
	dim := s size.
	
	"Creating return point - (0,0,...,0)"
	k := 1.
	coords := Array new: dim.
	[k <= dim] whileTrue:
		[coords at: k put: 0.
		k := k + 1.].
	pt := Punkt new.
	pt setCoords: coords setTorus: torus.

	"Inserting the point to initial dictionary that holds points"
	pts := Dictionary new.
	torus setPoints: pts.
	torus insertKey: coords withValue: pt.
	"Setting torus shape"
	torus setShape: s.
	"Returning initial point"
	^pt! !
!Torus class categoriesFor: #shape:!public! !

Spacer guid: (GUID fromString: '{7496455E-5CD7-4D31-BA72-ED21500E014E}')!
Spacer comment: ''!
!Spacer categoriesForClass!Collections-Abstract! !
!Spacer methodsFor!

% x
	|col elems k|
	col := OrderedCollection new.
	k := limit.
	self do:
		[:elem |
			elems := elem % x.
			col addAll: elems.
			k := k - 1.
			"Truncating a possible infinite walk"
			(k <= 0) ifTrue: [^Spacer return: col]].
	^Spacer return: col.!

, t
	|col k|
	col := OrderedCollection new.
	k := limit.
	self do:
		[:elem |
			col add: elem.
			k := k - 1.
			"Truncating a possible infinite walk"
			(k <= 0) ifTrue: [^Spacer return: col]].
	t do: 
		[:elem | 
			col add: elem.
			k := k - 1.
			"Truncating a possible infinite walk"
			(k <= 0) ifTrue: [^Spacer return: col]].
	^Spacer return: col!

| x
	|col elems k|
	col := OrderedCollection new.
	k := limit.
	self do:
		[:elem |
			elems := elem | x.
			col addAll: elems.
			k := k - 1.
			"Truncating a possible infinite walk"
			(k <= 0) ifTrue: [^Spacer return: col]].
	^Spacer return: col.!

add: newElement
	^self shouldNotImplement!

createPointCollection: collection
	pointCollection := collection.
	block := nil.
	point := nil.
	containsBlock := false.
	"A limit for the length of the walk - if inifite it will be truncated to normal"
	limit :=10000!

createPointCollection: collection withBlock: b andPoint: p
	pointCollection := collection.
	block := b.
	point := p.
	containsBlock := true.
	"A limit for the length of the walk - if inifite it will be truncated to normal"
	limit :=10000!

do: operation
	|w|
	containsBlock ifFalse: [^pointCollection do: operation].
	containsBlock ifTrue: [
		operation value: point.
		w := block value: point.
		(w = nil) ifFalse:[
			w do: operation
		]
		].!

first: anInteger
	"Implementation from moodle"
    | answer i |
    answer := OrderedCollection new.
    anInteger > 0 ifFalse: [^answer].
    i := anInteger.
    self do:
        [:each |
        answer add: each.
        i := i - 1.
        i = 0 ifTrue: [^answer]].
    ^answer!

remove: oldElement ifAbsent: exceptionHandler
	^self shouldNotImplement!

species
	^OrderedCollection! !
!Spacer categoriesFor: #%!public! !
!Spacer categoriesFor: #,!public! !
!Spacer categoriesFor: #|!public! !
!Spacer categoriesFor: #add:!public! !
!Spacer categoriesFor: #createPointCollection:!private! !
!Spacer categoriesFor: #createPointCollection:withBlock:andPoint:!private! !
!Spacer categoriesFor: #do:!public! !
!Spacer categoriesFor: #first:!public! !
!Spacer categoriesFor: #remove:ifAbsent:!public! !
!Spacer categoriesFor: #species!public! !

!Spacer class methodsFor!

new
	^self shouldNotImplement!

new: x
	^self shouldNotImplement!

return: collection
	^(self basicNew) createPointCollection: collection!

return: collection withBlock: b andPoint: p
	^(self basicNew) createPointCollection: collection withBlock: b andPoint: p! !
!Spacer class categoriesFor: #new!public! !
!Spacer class categoriesFor: #new:!public! !
!Spacer class categoriesFor: #return:!public! !
!Spacer class categoriesFor: #return:withBlock:andPoint:!public! !

"Binary Globals"!

