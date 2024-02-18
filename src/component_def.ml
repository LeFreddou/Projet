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

class texture =
  object
    val texture = Component.def (Texture.color (Gfx.color 0 0 0 0))
    method texture = texture
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



(*0 = camera
  1 = player
  2 = wall
  3 = zone*)
class layer =
  object 
    val layer = Component.def 0
    method layer = layer
  end

(*1 = moov BG
  2 = moov HD
  3 = téléportation entrée ?
  4 = tp sortie 
  5 = death zone*)
class effect =
  object
    val effect = Component.def 0
    method effect = effect
  end

class haut =
  object 
    val haut = Component.def false
    method haut = haut
  end


class bas =
  object 
    val bas = Component.def false
    method bas = bas
  end

class droite =
  object 
    val droite = Component.def false
    method droite = droite
  end
class gauche =
  object 
    val gauche = Component.def false
    method gauche = gauche
  end



(*Complex components*)

class zone_moov = 
  object
    inherit haut
    inherit bas
    inherit gauche
    inherit droite
  end

class drawable =
  object
    inherit position
    inherit rect
    inherit texture
    inherit layer
  end


class movable = 
  object 
    inherit position
    inherit velocity
    inherit zone_moov
    inherit layer
  end

class collidable =
  object 
    inherit position
    inherit velocity
    inherit layer
    inherit rect
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
    inherit! zone_moov
  end

class zone = 
  object 
    inherit drawable
    inherit id
    inherit! layer
    inherit! zonable
  end

class camera =
  object
    inherit drawable
    inherit! movable
    inherit id 
    inherit! layer
    inherit!  zonable
  end