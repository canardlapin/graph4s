package graph4s

object syntax:
  extension [V](left: V)
    infix def --(right: V): Link[V] =
      Link(left, right)

    infix def -->(right: V): ArcInput[V] =
      ArcInput(left, right)
