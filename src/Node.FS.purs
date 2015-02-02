module Node.FS where

import Data.Null
import Data.Object
import Data.Function
import Data.Function.Eff
import Data.Function.Callback
import Data.Function.Callback.Other
import Control.Monad.Eff

import Node.Buffer (Buffer())

import Node.Stream.Char
import Node.Events

foreign import data FS :: !

type FilePath = String
type DestPath = String
type FD = Number

type ByteNum = Number
type UID = Number
type GID = Number
type FileMode = Number
type FileTime = Number
type FileFlags = String
type SymlinkType = String

type BufferOffset = Number
type BufferLength = Number
type FDPosition = Number

type ReadFileOpts = {
  encoding :: N String,
  flag     :: N FileFlags
}

type WriteFileOpts = {
  encoding :: N String,
  flag     :: N FileFlags,
  mode     :: N FileMode
}

type ReadStreamOpts = {
  encoding  :: N String,
  flag      :: N FileFlags,
  fd        :: N FD,
  mode      :: N FileMode,
  autoClose :: N Boolean
}

type WriteStreamOpts = WriteFileOpts

type FileStats = {
  dev     :: Number,
  ino     :: Number,
  mode    :: Number,
  nlink   :: Number,
  uid     :: Number,
  gid     :: Number,
  rdev    :: Number,
  size    :: Number,
  blksize :: Number,
  blocks  :: Number,
  atime   :: Number,
  mtime   :: Number,
  ctime   :: Number
}

fsrename     = runFnCb2 nativeFS.rename
fstruncate   = runFnCb2 nativeFS.truncate
fschown      = runFnCb3 nativeFS.chown
fslchown     = runFnCb3 nativeFS.lchown
fschmod      = runFnCb2 nativeFS.chmod
fslchmod     = runFnCb2 nativeFS.lchmod
fsstat       = runFnCb1 nativeFS.stat
fslstat      = runFnCb1 nativeFS.lstat
fslink       = runFnCb2 nativeFS.link
fssymlink    = runFnCb3 nativeFS.symlink
fsreadlink   = runFnCb1 nativeFS.readlink
fsrealpath   = runFnCb2 nativeFS.realpath
fsunlink     = runFnCb1 nativeFS.unlink
fsrmdir      = runFnCb1 nativeFS.rmdir
fsmkdir      = runFnCb2 nativeFS.mkdir
fsreaddir    = runFnCb1 nativeFS.readdir
fsutimes     = runFnCb3 nativeFS.utimes

fsReadFile   = runFnCb2 nativeFS.readFile
fsWriteFile  = runFnCb3 nativeFS.writeFile
fsAppendFile = runFnCb3 nativeFS.appendFile
fsExists p cb = runFnEff2 nativeFS.exists p (asCb1 cb)

fsopen      = runFnCb3 nativeFS.open
fsftruncate = runFnCb2 nativeFS.ftruncate
fsfchown    = runFnCb3 nativeFS.fchown
fsfchmod    = runFnCb2 nativeFS.fchmod
fsfstat     = runFnCb1 nativeFS.fstat
fsclose     = runFnCb1 nativeFS.close
fsfutimes   = runFnCb3 nativeFS.futimes
fsfsync     = runFnCb1 nativeFS.fsync
fswrite     = runFnCb5 nativeFS.write
fsread      = runFnCb5 nativeFS.read

fsCreateReadStream = runFnEff2 nativeFS.createReadStream
fsCreateWriteStream = runFnEff2 nativeFS.createWriteStream

statsIs :: String -> FileStats -> Boolean
statsIs = runMethod0

statsIsFile            = statsIs "isFile"
statsIsDirectory       = statsIs "isDirectory"
statsIsBlockDevice     = statsIs "isBlockDevice"
statsIsCharacterDevice = statsIs "isCharacterDevice"
statsIsSymbolicLink    = statsIs "isSymbolicLink"
statsIsFIFO            = statsIs "isFIFO"
statsIsSocket          = statsIs "isSocket"

foreign import data FSReadStream :: * -> # ! -> *
foreign import data FSWriteStream :: * -> # ! -> *

instance fsReadStream :: ReadStreamChar (FSReadStream chars) chars
instance fsWriteStream :: WriteStreamChar (FSWriteStream chars) chars

foreign import nativeFS
  """var nativeFS = require('fs')"""
  :: {
  rename     :: forall eff. FnCb2 (fs :: FS | eff) FilePath DestPath Unit,
  truncate   :: forall eff. FnCb2 (fs :: FS | eff) FilePath ByteNum Unit,
  chown      :: forall eff. FnCb3 (fs :: FS | eff) FilePath UID GID Unit,
  lchown     :: forall eff. FnCb3 (fs :: FS | eff) FilePath UID GID Unit,
  chmod      :: forall eff. FnCb2 (fs :: FS | eff) FilePath FileMode Unit,
  lchmod     :: forall eff. FnCb2 (fs :: FS | eff) FilePath FileMode Unit,
  stat       :: forall eff. FnCb1 (fs :: FS | eff) FilePath FileStats,
  lstat      :: forall eff. FnCb1 (fs :: FS | eff) FilePath FileStats,
  link       :: forall eff. FnCb2 (fs :: FS | eff) FilePath DestPath Unit,
  symlink    :: forall eff. FnCb3 (fs :: FS | eff) FilePath DestPath (N SymlinkType) Unit,
  readlink   :: forall eff. FnCb1 (fs :: FS | eff) FilePath FilePath,
  realpath   :: forall eff cache. FnCb2 (fs :: FS | eff) FilePath (N {|cache}) FilePath,
  unlink     :: forall eff. FnCb1 (fs :: FS | eff) FilePath Unit,
  rmdir      :: forall eff. FnCb1 (fs :: FS | eff) FilePath Unit,
  mkdir      :: forall eff. FnCb2 (fs :: FS | eff) DestPath (N FileMode) Unit,
  readdir    :: forall eff. FnCb1 (fs :: FS | eff) FilePath [String],
  utimes     :: forall eff. FnCb3 (fs :: FS | eff) FilePath FileTime FileTime Unit,
  readFile   :: forall eff repr. FnCb2 (fs :: FS | eff) FilePath (N ReadFileOpts) repr,
  writeFile  :: forall eff repr. FnCb3 (fs :: FS | eff) DestPath repr (N WriteFileOpts) Unit,
  appendFile :: forall eff repr. FnCb3 (fs :: FS | eff) DestPath repr (N WriteFileOpts) Unit,
  exists     :: forall eff. FnEff2 (fs :: FS | eff) FilePath (Cb1 Boolean) Unit,
  --
  open      :: forall eff. FnCb3 (fs :: FS | eff) FilePath FileFlags (N FileMode) FD,
  ftruncate :: forall eff. FnCb2 (fs :: FS | eff) FD ByteNum Unit,
  fchown    :: forall eff. FnCb3 (fs :: FS | eff) FD UID GID Unit,
  fchmod    :: forall eff. FnCb2 (fs :: FS | eff) FD FileMode Unit,
  fstat     :: forall eff. FnCb1 (fs :: FS | eff) FD FileStats,
  close     :: forall eff. FnCb1 (fs :: FS | eff) FD Unit,
  futimes   :: forall eff. FnCb3 (fs :: FS | eff) FD FileTime FileTime Unit,
  fsync     :: forall eff. FnCb1 (fs :: FS | eff) FD Unit,
  write     :: forall eff. FnCb5 (fs :: FS | eff) FD Buffer BufferOffset BufferLength FDPosition ByteNum,
  read      :: forall eff. FnCb5 (fs :: FS | eff) FD Buffer BufferOffset BufferLength FDPosition ByteNum,
  --
  createReadStream :: forall eff repr. FnEff2 (fs :: FS | eff) FilePath (N ReadStreamOpts) (FSReadStream repr eff),
  createWriteStream :: forall eff repr. FnEff2 (fs :: FS | eff) FilePath (N WriteStreamOpts) (FSWriteStream repr eff),
  --
  renameSync     :: forall eff. FnEff2 (fs :: FS | eff) FilePath DestPath Unit,
  truncateSync   :: forall eff. FnEff2 (fs :: FS | eff) FilePath ByteNum Unit,
  chownSync      :: forall eff. FnEff3 (fs :: FS | eff) FilePath UID GID Unit,
  lchownSync     :: forall eff. FnEff3 (fs :: FS | eff) FilePath UID GID Unit,
  chmodSync      :: forall eff. FnEff2 (fs :: FS | eff) FilePath FileMode Unit,
  lchmodSync     :: forall eff. FnEff2 (fs :: FS | eff) FilePath FileMode Unit,
  statSync       :: forall eff. FnEff1 (fs :: FS | eff) FilePath FileStats,
  lstatSync      :: forall eff. FnEff1 (fs :: FS | eff) FilePath FileStats,
  linkSync       :: forall eff. FnEff2 (fs :: FS | eff) FilePath DestPath Unit,
  symlinkSync    :: forall eff. FnEff3 (fs :: FS | eff) FilePath DestPath (N SymlinkType) Unit,
  readlinkSync   :: forall eff. FnEff1 (fs :: FS | eff) FilePath FilePath,
  realpathSync   :: forall eff cache. FnEff2 (fs :: FS | eff) FilePath (N {|cache}) FilePath,
  unlinkSync     :: forall eff. FnEff1 (fs :: FS | eff) FilePath Unit,
  rmdirSync      :: forall eff. FnEff1 (fs :: FS | eff) FilePath Unit,
  mkdirSync      :: forall eff. FnEff2 (fs :: FS | eff) DestPath (N FileMode) Unit,
  readdirSync    :: forall eff. FnEff1 (fs :: FS | eff) FilePath [String],
  utimesSync     :: forall eff. FnEff3 (fs :: FS | eff) FilePath FileTime FileTime Unit,
  readFileSync   :: forall eff repr. FnEff2 (fs :: FS | eff) FilePath (N ReadFileOpts) repr,
  writeFileSync  :: forall eff repr. FnEff3 (fs :: FS | eff) DestPath repr (N WriteFileOpts) Unit,
  appendFileSync :: forall eff repr. FnEff3 (fs :: FS | eff) DestPath repr (N WriteFileOpts) Unit,
  existsSync     :: forall eff. FnEff1 (fs :: FS | eff) FilePath Boolean,
  --
  openSync      :: forall eff. FnEff3 (fs :: FS | eff) FilePath FileFlags (N FileMode) FD,
  ftruncateSync :: forall eff. FnEff2 (fs :: FS | eff) FD ByteNum Unit,
  fchownSync    :: forall eff. FnEff3 (fs :: FS | eff) FD UID GID Unit,
  fchmodSync    :: forall eff. FnEff2 (fs :: FS | eff) FD FileMode Unit,
  fstatSync     :: forall eff. FnEff1 (fs :: FS | eff) FD FileStats,
  closeSync     :: forall eff. FnEff1 (fs :: FS | eff) FD Unit,
  futimesSync   :: forall eff. FnEff3 (fs :: FS | eff) FD FileTime FileTime Unit,
  fsyncSync     :: forall eff. FnEff1 (fs :: FS | eff) FD Unit,
  writeSync     :: forall eff. FnEff5 (fs :: FS | eff) FD Buffer BufferOffset BufferLength FDPosition ByteNum,
  readSync      :: forall eff. FnEff5 (fs :: FS | eff) FD Buffer BufferOffset BufferLength FDPosition ByteNum
  }
