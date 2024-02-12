open Ecs

(*basic components*)
class position =
  object
    val pos = Component.def Vector.zero
    method pos = pos
  end

class rect =
  object
    val rect = Component.def Rect.{width = 0; height = 0}
    method rect = rect
  end

class velocity =
  object
    val velocity = Component.def Vector.zero
    method velocity = velocity
  end

class color =
  object
    val color = Component.def (Gfx.color 0 0 0 0)
    method color = color
  end

class id =
  object
    val id = Component.def ("")
    method id = id
  end

class sibling =
  object 
    val sibling = Component.def ("")
    method sibling = sibling
  end

class moov_up_left =
  object 
    val moov_up_left = Component.def true
    method moov_up_left = moov_up_left
  end


(*1 = player
  2 = wall
  3 = zone*)
class layer =
  object 
    val layer = Component.def 0
    method layer = layer
  end

(*1 = moov HD
  2 = moov BG
  3 = téléportation entrée ?
  4 = tp sortie ?*)
class effect =
  object
    val effect = Component.def 0
    method effect = effect
  end
(*Complex components*)

class drawable =
  object
    inherit position
    inherit rect
    inherit color
  end


class movable = 
  object 
    inherit position
    inherit velocity
    inherit moov_up_left
  end

class collidable =
  object 
    inherit position
    inherit velocity
    inherit layer
    inherit rect
  end

class zone_moov = 
  object
    inherit moov_up_left
  end

class zone_tp =
  object 
    inherit sibling
  end

class zonable =
  object 
    inherit id
    inherit position
    inherit rect
    inherit layer
    inherit effect
    inherit zone_moov
    inherit zone_tp

  end

class wall =
  object
    inherit drawable
    inherit id
    inherit! collidable
  end

class player = 
  object 
    inherit drawable
    inherit! movable
    inherit id
    inherit! collidable
    inherit! zonable 
    inherit! moov_up_left
  end

class zone = 
  object 
    inherit drawable
    inherit id
    inherit layer
    inherit! zonable
  end