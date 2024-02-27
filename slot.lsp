;(slot <object> <slot>)
;	This facilitates Object-Oriented programming. All 
;	RefLisp objects are deemed to belong to a class. To find
;	the class of an object you call (slot) thusly:
;
;	(slot <object> 'class)
;
;	When <object> is not an symbol, or if the symbol has no 'class
;	property, the function returns a symbol corresponding to the
;	Lisp basic type, from this list:
;
;		cons, fixnum, subr, fsubr, string, stream, 
;		symbol, float
;
;	Example:
;		(slot 34.7 'class) ==> float
;		(slot '(1 2) 'class) ==> cons
;			
;	If the object is an symbol the class is assumed 
;	to be the value of its 'class property. If it has
;	no 'class property then (slot) returns SYMBOL.
;
;	If <slot> is not equal to 'class then (slot) looks
;	in the object's property list for a property of the same name. 
;	If it cannot find one, then it fetches the super-classes of the 
;	object, and looks in the super-class's object's property list, 
;	and so on recursivly for all the super-classes' super-classes.
;
;	On object's super-classes are the list of symbols contained in 
;	it's "super-classes" property. Thus:
;
;		(put 'cat 'super-classes '(mammal pet))
;	
;	declares to (slot) that it should search 'mammal and  'pet when
;	looking for slots. 
;
;	Note that 'cat is really a "meta-object", an object which
;	describes others. So the class of 'cat would probably be 'class,
;	in a normal system.  The purpose of (slot) is to provide basic 
;	search facilities, rather than to implement an entire OO system.
;			
;	Example:
;		(put 'mammal 'skin 'fur)  	; mammals are furry
;		(put 'toon 'lives '5000)  	; toons are well hard
;		(put 'pet 'cost '$50) 		; pets cost money
;		(put 'cat 'super-classes 	; cats are mammals
;			'(mammal pet))		; and pets
;		(put 'cat 'lives '9)  		; cats only have nine
;						
;		(put 'felix 'super-classes '(toon cat)) ; felix is a cat
;							; and a cartoon.
;		; What is felix's skin like?
;		(slot 'felix 'skin) ; ==> fur
;		; How much does felix cost?
;		(slot 'felix 'cost) ; ==> $50
;		; Does this cat have nine lives?
;		(slot 'felix 'lives) ; ==> 5000
;
;	This implements a form of multiple inheritance, in that (slot)
;	looks through a list of super-classes when searching. (slot)
;	searches the current level of super-classes in a depth-first
;	manner, in the order in which the super-classes appear.
;	Thus, in the example, felix being a toon takes priotity over his
;	cat nature. In the example the search order is:
;
;		felix
;		toon
;		cat
;		mammal
;		pet
;
;	To build a fully OO system, you need to implement inheritance 
;	at the time new objects are instantiated. (slot) simply provides a
;	quick lookup routine to bootstrap the inheritance system of your
;	choice. See the file "smalltalk.lsp" for an elaborate system.
;
;	Example:
;		; Use green for all integers
;		(put 'fixnum 'color 'green)
;		(slot 35 'color) ==> green
;		(slot 346 'color) ==> green

(defun slot (obj slt)
  (cond
    ((symbolp obj)
     (cond
       ((eq slt 'class)
	(get obj 'class 'symbol))
       ((get obj slt))
       (t
	(slot-aux (get obj 'super-classes) slt))))
    ((eq slt 'class)
     (cond
       ((stringp obj) 'string)
       ((numberp obj) 'number)
       ((consp obj) 'cons)
       ((streamp obj) 'stream)))
    (t
      nil)))

(defun slot-aux (super-classes slt)
  (do-loop
    (when (atom super-classes)
      (return))
    ((slot (pop super-classes) slt))))

(put 'mammal 'skin 'fur)  	; mammals are furry
(put 'toon 'lives '5000)  	; toons are well hard
(put 'pet 'cost '$50) 		; pets cost money
(put 'cat 'super-classes 	; cats are mammals
	'(mammal pet))			; and pets
(put 'cat 'lives '9)  		; cats only have nine

(put 'felix 'super-classes '(toon cat)) ; felix is a cat and a cartoon

