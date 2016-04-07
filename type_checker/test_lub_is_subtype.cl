class Z { };

class Z_Y inherits Z { };

class Z_Y_W inherits Z_Y { };

class Z_Y_V inherits Z_Y { };

class Z_X inherits Z { };



class C1 inherits C {
	c : Int;
};

class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};
