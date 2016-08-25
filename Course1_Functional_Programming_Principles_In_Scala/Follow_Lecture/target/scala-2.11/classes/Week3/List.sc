import Week3.{List, Nil, Cons}

def singleton[T](elem: T) = Cons[T](elem, new Nil[T])