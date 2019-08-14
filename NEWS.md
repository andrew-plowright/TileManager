# TileManager 0.3.0

Major update to TileManager! **This version is not backwards compatible**. View new README file for a tour of the new features.


# TileManager 0.3.0

Removed the `TileInput`, `TileApply`, `TempTiles` and `removeTempTiles` functions. These functions were intended to help incorporate tiling into other packages, but I have concluded that they were poorly thought-out and that it is best to allow users to decide exactly how they want to implement tiling. With that in mind, I would like to make `TileScheme` the main focus of this package.

# TileManager 0.2.0

Added an S3 `tileScheme` object class with a `plot` method.

# TileManager 0.1.1

Replaced 'paste' with 'file.path' for constructing file paths in the 'TempTiles' and 'removeTempTiles' functions. This will hopefully fix a bug whereby temporary files are left behind on POSIX file systems.

# TileManager 0.1.0

* Initial release of TileManager



