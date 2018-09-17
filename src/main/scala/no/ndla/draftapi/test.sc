val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)



l.takeWhile(_ < 4)
l.lift(-1)
l.takeRight(l.length - l.indexOf(4) - 1)
