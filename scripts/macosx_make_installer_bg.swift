#!/usr/bin/env swift
// Generate the installer left-pane background PNG from a source
// icon by multiplying its alpha channel (to make it blend into the
// pane) and re-tagging the DPI (to make Installer.app render it at
// a smaller point size).  The output PNG is committed as a binary
// asset so the build does not depend on Swift / ImageIO.
//
// Usage: macosx_make_installer_bg.swift <source.png> <output.png>
//
// Re-run this if you want to update the icon, the opacity, or the
// rendered size.

import Foundation
import CoreGraphics
import ImageIO

// 0.5 = half-transparent (blends with both light and dark installer
// background).  144 dpi = 256 px renders as ~128 pt, which fits in
// the ~150 pt left pane with a bit of breathing room.
let opacity: CGFloat = 0.5
let dpi: CGFloat = 100

let args = CommandLine.arguments
guard args.count == 3 else {
    FileHandle.standardError.write(
        "Usage: \(args[0]) <source.png> <output.png>\n".data(using: .utf8)!)
    exit(1)
}
let srcURL = URL(fileURLWithPath: args[1])
let dstURL = URL(fileURLWithPath: args[2])

guard let src = CGImageSourceCreateWithURL(srcURL as CFURL, nil),
      let img = CGImageSourceCreateImageAtIndex(src, 0, nil) else {
    FileHandle.standardError.write("Cannot read \(srcURL.path)\n".data(using: .utf8)!)
    exit(1)
}

let w = img.width
let h = img.height
let cs = CGColorSpaceCreateDeviceRGB()
guard let ctx = CGContext(data: nil, width: w, height: h,
                          bitsPerComponent: 8, bytesPerRow: 0,
                          space: cs,
                          bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else {
    FileHandle.standardError.write("Cannot create context\n".data(using: .utf8)!)
    exit(1)
}
ctx.setAlpha(opacity)
ctx.draw(img, in: CGRect(x: 0, y: 0, width: w, height: h))
guard let faded = ctx.makeImage() else {
    FileHandle.standardError.write("makeImage failed\n".data(using: .utf8)!)
    exit(1)
}

guard let dest = CGImageDestinationCreateWithURL(
        dstURL as CFURL, "public.png" as CFString, 1, nil) else {
    FileHandle.standardError.write("Cannot create destination\n".data(using: .utf8)!)
    exit(1)
}
let props: [CFString: Any] = [
    kCGImagePropertyDPIWidth:  dpi,
    kCGImagePropertyDPIHeight: dpi,
]
CGImageDestinationAddImage(dest, faded, props as CFDictionary)
guard CGImageDestinationFinalize(dest) else {
    FileHandle.standardError.write("finalize failed\n".data(using: .utf8)!)
    exit(1)
}
