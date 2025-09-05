/** This package contains [[Color]] constants. They include the colors listed in the W3C’s
  * [[https://www.w3.org/TR/css-color-4/ CSS Color Module specification]] (version December,
  * 2021), plus some additional ones.
  *
  * All the colors are fully opaque, except [[colors.Transparent Transparent]], which is
  * fully transparent.
  *
  * The contents of this package are aliased in the top-level package [[o1]] so that
  * they are accessible to students simply via `import o1.*`. */
package o1.gui.colors

import o1.gui.Color

/** Represents a fully transparent (white) color. */
val Transparent: Color = Color(0xffffff00, "transparent")

/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val AliceBlue: Color = Color(0xf0f8ffff, "alice blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val AlizarinCrimson: Color = Color(0x4e1500ff, "alizarin crimson")
/** A named color as per, um, some standard or other, maybe. */
val Amethyst: Color = Color(0x9966ccff, "amethyst")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val AntiqueWhite: Color = Color(0xfaebd7ff, "antique white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Aqua: Color = Color(0x00ffffff, "aqua")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Aquamarine: Color = Color(0x7fffd4ff, "aquamarine")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Azure: Color = Color(0xf0ffffff, "azure")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Beige: Color = Color(0xf5f5dcff, "beige")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Bisque: Color = Color(0xffe4c4ff, "bisque")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Black: Color = Color(0x000000ff, "black")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BlanchedAlmond: Color = Color(0xffebcdff, "blanched almond")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Blue: Color = Color(0x0000ffff, "blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BlueViolet: Color = Color(0x8a2be2ff, "blue violet")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val BrightRed: Color = Color(0xdb0000ff, "bright red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Brown: Color = Color(0xa52a2aff, "brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val BurlyWood: Color = Color(0xdeb887ff, "burly wood")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val CadetBlue: Color = Color(0x5f9ea0ff, "cadet blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val CadmiumYellow: Color = Color(0xffec00ff, "cadmium yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Chartreuse: Color = Color(0x7fff00ff, "chartreuse")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Chocolate: Color = Color(0xd2691eff, "chocolate")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Coral: Color = Color(0xff7f50ff, "coral")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val CornflowerBlue: Color = Color(0x6495edff, "cornflower blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Cornsilk: Color = Color(0xfff8dcff, "cornsilk")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Crimson: Color = Color(0xdc143cff, "crimson")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Cyan: Color = Color(0x00ffffff, "cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkBlue: Color = Color(0x00008bff, "dark blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkCyan: Color = Color(0x008b8bff, "dark cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGoldenrod: Color = Color(0xb8860bff, "dark goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGray: Color = Color(0xa9a9a9ff, "dark gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGreen: Color = Color(0x006400ff, "dark green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkGrey: Color = Color(0xa9a9a9ff, "dark grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkKhaki: Color = Color(0xbdb76bff, "dark khaki")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkMagenta: Color = Color(0x8b008bff, "dark magenta")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOliveGreen: Color = Color(0x556b2fff, "dark olive green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOrange: Color = Color(0xff8c00ff, "dark orange")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkOrchid: Color = Color(0x9932ccff, "dark orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkRed: Color = Color(0x8b0000ff, "dark red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSalmon: Color = Color(0xe9967aff, "dark salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSeaGreen: Color = Color(0x8fbc8fff, "dark sea green")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val DarkSienna: Color = Color(0x5f2e1fff, "dark sienna")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateBlue: Color = Color(0x483d8bff, "dark slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateGray: Color = Color(0x2f4f4fff, "dark slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkSlateGrey: Color = Color(0x2f4f4fff, "dark slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkTurquoise: Color = Color(0x00ced1ff, "dark turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DarkViolet: Color = Color(0x9400d3ff, "dark violet")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DeepPink: Color = Color(0xff1493ff, "deep pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DeepSkyBlue: Color = Color(0x00bfffff, "deep sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DimGray: Color = Color(0x696969ff, "dim gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DimGrey: Color = Color(0x696969ff, "dim grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val DodgerBlue: Color = Color(0x1e90ffff, "dodger blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val FireBrick: Color = Color(0xb22222ff, "fire brick")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val FloralWhite: Color = Color(0xfffaf0ff, "floral white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val ForestGreen: Color = Color(0x228b22ff, "forest green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Fuchsia: Color = Color(0xff00ffff, "fuchsia")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gainsboro: Color = Color(0xdcdcdcff, "gainsboro")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val GhostWhite: Color = Color(0xf8f8ffff, "ghost white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gold: Color = Color(0xffd700ff, "gold")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Goldenrod: Color = Color(0xdaa520ff, "goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Gray: Color = Color(0x808080ff, "gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Green: Color = Color(0x008000ff, "green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val GreenYellow: Color = Color(0xadff2fff, "green yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Grey: Color = Color(0x808080ff, "grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Honeydew: Color = Color(0xf0fff0ff, "honeydew")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val HotPink: Color = Color(0xff69b4ff, "hot pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val IndianRed: Color = Color(0xcd5c5cff, "indian red")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val IndianYellow: Color = Color(0xffb800ff, "indian yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Indigo: Color = Color(0x4b0082ff, "indigo")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Ivory: Color = Color(0xfffff0ff, "ivory")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Khaki: Color = Color(0xf0e68cff, "khaki")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Lavender: Color = Color(0xe6e6faff, "lavender")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LavenderBlush: Color = Color(0xfff0f5ff, "lavender blush")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LawnGreen: Color = Color(0x7cfc00ff, "lawn green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LemonChiffon: Color = Color(0xfffacdff, "lemon chiffon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightBlue: Color = Color(0xadd8e6ff, "light blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightCoral: Color = Color(0xf08080ff, "light coral")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightCyan: Color = Color(0xe0ffffff, "light cyan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGoldenrodYellow: Color = Color(0xfafad2ff, "light goldenrod yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGray: Color = Color(0xd3d3d3ff, "light gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGreen: Color = Color(0x90ee90ff, "light green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightGrey: Color = Color(0xd3d3d3ff, "light grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightPink: Color = Color(0xffb6c1ff, "light pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSalmon: Color = Color(0xffa07aff, "light salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSeaGreen: Color = Color(0x20b2aaff, "light sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSkyBlue: Color = Color(0x87cefaff, "light sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSlateGray: Color = Color(0x778899ff, "light slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSlateGrey: Color = Color(0x778899ff, "light slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightSteelBlue: Color = Color(0xb0c4deff, "light steel blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LightYellow: Color = Color(0xffffe0ff, "light yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Lime: Color = Color(0x00ff00ff, "lime")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val LimeGreen: Color = Color(0x32cd32ff, "lime green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Linen: Color = Color(0xfaf0e6ff, "linen")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Magenta: Color = Color(0xff00ffff, "magenta")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Maroon: Color = Color(0x800000ff, "maroon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumAquamarine: Color = Color(0x66cdaaff, "medium aquamarine")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumBlue: Color = Color(0x0000cdff, "medium blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumOrchid: Color = Color(0xba55d3ff, "medium orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumPurple: Color = Color(0x9370dbff, "medium purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSeaGreen: Color = Color(0x3cb371ff, "medium sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSlateBlue: Color = Color(0x7b68eeff, "medium slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumSpringGreen: Color = Color(0x00fa9aff, "medium spring green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumTurquoise: Color = Color(0x48d1ccff, "medium turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MediumVioletRed: Color = Color(0xc71585ff, "medium violet red")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val MidnightBlack: Color = Color(0x000000ff, "midnight black")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MidnightBlue: Color = Color(0x191970ff, "midnight blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MintCream: Color = Color(0xf5fffaff, "mint cream")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val MistyRose: Color = Color(0xffe4e1ff, "misty rose")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Moccasin: Color = Color(0xffe4b5ff, "moccasin")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val NavajoWhite: Color = Color(0xffdeadff, "navajo white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Navy: Color = Color(0x000080ff, "navy")
/** A named color as per [[http://montypython.50webs.com/scripts/Series_1/53.htm the MP standard]]. */
lazy val NorwegianBlue: Color = deadParrot()
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OldLace: Color = Color(0xfdf5e6ff, "old lace")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Olive: Color = Color(0x808000ff, "olive")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OliveDrab: Color = Color(0x6b8e23ff, "olive drab")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Orange: Color = Color(0xffa500ff, "orange")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val OrangeRed: Color = Color(0xff4500ff, "orange red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Orchid: Color = Color(0xda70d6ff, "orchid")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleGoldenrod: Color = Color(0xeee8aaff, "pale goldenrod")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleGreen: Color = Color(0x98fb98ff, "pale green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleTurquoise: Color = Color(0xafeeeeff, "pale turquoise")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PaleVioletRed: Color = Color(0xdb7093ff, "pale violet red")
/** A named color [[https://en.wikipedia.org/wiki/Pantone_448_C famous  for  being  really  ugly]]. */
val Pantone448C: Color = Color(0x4a412aff, "Pantone 448 C")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PapayaWhip: Color = Color(0xffefd5ff, "papaya whip")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PeachPuff: Color = Color(0xffdab9ff, "peach puff")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Peru: Color = Color(0xcd853fff, "peru")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PhthaloBlue: Color = Color(0x0c0040ff, "phthalo blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PhthaloGreen: Color = Color(0x102e3cff, "phthalo green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Pink: Color = Color(0xffc0cbff, "pink")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Plum: Color = Color(0xdda0ddff, "plum")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val PowderBlue: Color = Color(0xb0e0e6ff, "powder blue")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val PrussianBlue: Color = Color(0x021e44ff, "Prussian blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Purple: Color = Color(0x800080ff, "purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RebeccaPurple: Color = Color(0x663399ff, "rebecca purple")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Red: Color = Color(0xff0000ff, "red")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RosyBrown: Color = Color(0xbc8f8fff, "rosy brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val RoyalBlue: Color = Color(0x4169e1ff, "royal blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SaddleBrown: Color = Color(0x8b4513ff, "saddle brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Salmon: Color = Color(0xfa8072ff, "salmon")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SandyBrown: Color = Color(0xf4a460ff, "sandy brown")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val SapGreen: Color = Color(0x0a3410ff, "sap green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SeaGreen: Color = Color(0x2e8b57ff, "sea green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Seashell: Color = Color(0xfff5eeff, "seashell")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Sienna: Color = Color(0xa0522dff, "sienna")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Silver: Color = Color(0xc0c0c0ff, "silver")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SkyBlue: Color = Color(0x87ceebff, "sky blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateBlue: Color = Color(0x6a5acdff, "slate blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateGray: Color = Color(0x708090ff, "slate gray")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SlateGrey: Color = Color(0x708090ff, "slate grey")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Snow: Color = Color(0xfffafaff, "snow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SpringGreen: Color = Color(0x00ff7fff, "spring green")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val SteelBlue: Color = Color(0x4682b4ff, "steel blue")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Tan: Color = Color(0xd2b48cff, "tan")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Teal: Color = Color(0x008080ff, "teal")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Thistle: Color = Color(0xd8bfd8ff, "thistle")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val TitaniumHwite: Color = Color(0xffffffff, "titanium hwite")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Tomato: Color = Color(0xff6347ff, "tomato")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Turquoise: Color = Color(0x40e0d0ff, "turquoise")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val VanDykeBrown: Color = Color(0x221b15ff, "Van Dyke brown")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Violet: Color = Color(0xee82eeff, "violet")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Wheat: Color = Color(0xf5deb3ff, "wheat")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val White: Color = Color(0xffffffff, "white")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val WhiteSmoke: Color = Color(0xf5f5f5ff, "white smoke")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val Yellow: Color = Color(0xffff00ff, "yellow")
/** A named color as per [[https://www.w3.org/TR/css-color-4/ W3C’s list of colors]]. */
val YellowGreen: Color = Color(0x9acd32ff, "yellow green")
/** A named color as per [[https://thomaspark.co/2015/11/bob-ross-color-palette-in-css/ Bob Ross]]. */
val YellowOchre: Color = Color(0xc79b00ff, "yellow ochre")


/** A grayscale color that contains 10% black. */
val Black10: Color = Black.lighter(90).withName("black10")
/** A grayscale color that contains 20% black. */
val Black20: Color = Black.lighter(80).withName("black20")
/** A grayscale color that contains 30% black. */
val Black30: Color = Black.lighter(70).withName("black30")
/** A grayscale color that contains 40% black. */
val Black40: Color = Black.lighter(60).withName("black40")
/** A grayscale color that contains 50% black. */
val Black50: Color = Black.lighter(50).withName("black50")
/** A grayscale color that contains 60% black. */
val Black60: Color = Black.lighter(40).withName("black60")
/** A grayscale color that contains 70% black. */
val Black70: Color = Black.lighter(30).withName("black70")
/** A grayscale color that contains 80% black. */
val Black80: Color = Black.lighter(20).withName("black80")
/** A grayscale color that contains 90% black. */
val Black90: Color = Black.lighter(10).withName("black90")
/** A grayscale color that contains 10% white. */
val White10: Color = White.darker(90).withName("white10")
/** A grayscale color that contains 20% white. */
val White20: Color = White.darker(80).withName("white20")
/** A grayscale color that contains 30% white. */
val White30: Color = White.darker(70).withName("white30")
/** A grayscale color that contains 40% white. */
val White40: Color = White.darker(60).withName("white40")
/** A grayscale color that contains 50% white. */
val White50: Color = White.darker(50).withName("white50")
/** A grayscale color that contains 60% white. */
val White60: Color = White.darker(40).withName("white60")
/** A grayscale color that contains 70% white. */
val White70: Color = White.darker(30).withName("white70")
/** A grayscale color that contains 80% white. */
val White80: Color = White.darker(20).withName("white80")
/** A grayscale color that contains 90% white. */
val White90: Color = White.darker(10).withName("white90")

private def deadParrot(): Nothing =
  import o1.util.nice.seq.*
  val reasons = Seq("stone dead", "definitely deceased", "bleeding demised", "passed on", "no more",
                    "ceased to be", "expired and gone to meet its maker", "a stiff", "bereft of life",
                    "resting in peace", "pushing up the daisies", "metabolic processes are now history",
                    "off the twig", "kicked the bucket", "shuffled off its mortal coil",
                    "run down the curtain and joined the bleeding choir invisible", "an ex-color")
  throw NotImplementedError("This color’s " + reasons.randomElement() + "!")


