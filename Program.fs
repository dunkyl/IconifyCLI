open System
open System.IO

open System.Text.RegularExpressions

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.Memory

open System.Collections.Generic
open System.Runtime.InteropServices

let reverseEndian = false

let ushortBytes (x: IConvertible) =
    let mutable b = BitConverter.GetBytes (x.ToUInt16(null))
    if reverseEndian then Array.Reverse b
    b

let uintBytes (x: IConvertible) =
    let mutable b = BitConverter.GetBytes (x.ToUInt32(null))
    if reverseEndian then Array.Reverse b
    b

let writeIco (images: 'T Image list) outname =

    let outfile = File.OpenWrite(outname)

    let imageBuffers = [
        for image in images ->
            let ms = new MemoryStream()
            image.SaveAsPng(ms)
            ms.ToArray()
    ]

    let data = Array.concat [
        // HEADER
        yield! [
            0; 1 // constants for ICO
            images.Length
        ] |> List.map ushortBytes

        // ENTRIES
        for i, image in List.indexed images do
            let offset = 
                6 + 0x10 * images.Length + List.sumBy Array.length imageBuffers.[0..i-1]
            yield! [
                [|
                    byte(image.Width)
                    byte(image.Height)
                    0uy // PNG Color Palette Count
                    0uy // Reserved bytes in entry
                |]
                ushortBytes 1 // PNG Color Planes
                ushortBytes image.PixelType.BitsPerPixel // 32, usually
                uintBytes imageBuffers.[i].Length
                uintBytes offset     
            ]

        // IMAGE DATA
        yield! imageBuffers
    ]

    outfile.Write(ReadOnlySpan(data))
        
let ICON_SIZES = [
    16
    32
    48
    256
]

let resized (image: Image<'T>) size =
    let maxdim = max image.Height image.Width
    let x, y = 
        if image.Height > image.Width then
            (image.Height-image.Width) /2, 0
        else
            0, (image.Width-image.Height) /2

    let outputImage = new Image<'T>(maxdim, maxdim)
    outputImage.Mutate (
        fun i ->
            i.DrawImage(image, new Point(x, y), 1.0f)
             .Resize(size, size)
             |> ignore
    )
    outputImage

// FROM: http://www.fssnip.net/29/title/Regular-expression-active-pattern
// Daniel Robinson
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some([ for g in m.Groups -> g.Value ])
    else None

let println = printfn "%s"
type ImageType = Image<PixelFormats.Argb32>

type ArgExpr = 
| Default
| Output
| Specific of size: int

let rec parseArgs (tokens: string list) =
    match tokens with
    | [] -> []

    // specified params
    | "-o"::path::rest
    | "--output"::path::rest -> (Output, path)::(parseArgs rest)
    | "-d"::path::rest
    | "--default"::path::rest -> (Default, path)::(parseArgs rest)

    // procedural params 
    | (Regex @"\-[\d,]+" [givenSizes])::path::rest ->
        let givenSizes' = givenSizes.Substring 1
        let splitSizes = 
            if givenSizes.Contains ',' then
                (givenSizes').Split(',')
            else
                [|givenSizes'|]
        let mutable dones = []
        let parsedSizes =
            [
                for size in splitSizes ->
                    let size'' = size
                    let isInt, size' = Int32.TryParse(size'')
                    if (List.contains size' dones) then
                        raise (System.ArgumentException $"Size override '{size'}' was given more than once.")
                    else if isInt && (List.contains size' ICON_SIZES) then
                        dones <- size'::dones
                        (Specific size', path)
                    else
                        raise (System.ArgumentException $"Given size override '{size'}' is not a supported icon size.")
            ]
        List.concat [parsedSizes; parseArgs rest]

    | path::rest -> (Default, path)::(parseArgs rest)

let HELP =
    "Convert an image or set of images to a Windows ICO image.
Usage:
    iconify -d? [default image] (-[sizes,] [size-specific override])* (-o [output file])?
Examples:
    iconify example.png
    iconify example.png -16 example16px.bmp -o ../out.ico
Options:
    --default, -d : Explicitly specify a default image to use for sizes.
    --output, -o : Explicitly specify an output path. If omitted, inferred from default image.
    -[size] : Specify an image to use instead of the default at a particular size."

[<EntryPoint>]
let main (argv) =

    if argv = [||] then
        println HELP
        exit(-1)
    
    try
        let options = argv |> List.ofArray |> parseArgs |> Map.ofList
        let outputFile = Option.defaultValue options.[Default] (options.TryFind Output)

        for option in options do
            match option.Key with
            | Default ->
                printfn "Default: %s" option.Value
            | Output ->
                printfn "Output: %s" option.Value
            | Specific size ->
                printfn "Size %d: %s" size option.Value

        let newName = outputFile.Substring(0, outputFile.LastIndexOf '.')+".ico"

        let imagesForIcon = [
            for size in ICON_SIZES ->
                let path = Option.defaultValue options.[Default] (options.TryFind (Specific size))
                if not(File.Exists path) then
                    raise(System.IO.FileNotFoundException $"Input image '{path}' not found!")
                let img: ImageType = ImageType.Load(path)
                resized img size
        ]

        writeIco imagesForIcon newName
        
        0
    with
    | :? System.IO.FileNotFoundException
    | :? System.ArgumentException as e ->
        println e.Message
        -1
    | :? System.Collections.Generic.KeyNotFoundException as e->
        println "An output file name or default image file must be specified."
        -1
    | :? UnknownImageFormatException ->
        println "Only JPEG, PNG, BMP, GIF, and TARGA image formats are supported."
        -1
    | :? InvalidImageContentException as e ->
        println $"An error occurred reading an image:\n{e.Message}"
        -1