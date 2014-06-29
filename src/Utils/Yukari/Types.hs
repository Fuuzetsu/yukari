{-# LANGUAGE TemplateHaskell #-}
module Utils.Yukari.Types where

import Control.Lens
import Network.URI


type SiteTitle = String

type InfoSite = (SiteTitle, URI)

data ABTorrent = ABTorrent { _torrentID :: Integer
                           , _torrentInfoSuffix :: String
                           , _torrentInfo :: Maybe Information
                           , _torrentDownloadURI :: String
                           , _torrentURI :: String
                           , _torrentSize :: Integer
                           , _torrentSnatched :: Integer
                           , _torrentSeeders :: Integer
                           , _torrentLeechers :: Integer
                           } deriving (Show, Eq)


data ABTorrentGroup = ABTorrentGroup { _torrentName :: String
                                     , _torrentCategory :: Maybe Category
                                     --, torrentSynonyms :: String
                                     --, torrentSnatches :: Integer
                                     --, torrentComments :: Integer
                                     , _seriesID :: Integer
                                     , _groupID :: Integer
                                     , _torrentImageURI :: String
                                     --, torrentMoreInfo :: [InfoSite]
                                     , _torrentTags :: [String]
                                     , _torrents :: [ABTorrent]
                                     } deriving (Show, Eq)


data Information = AnimeInformation AnimeInfo
                 | MangaInformation MangaInfo
                 deriving (Show, Eq)

data AnimeInfo = AnimeInfo { _releaseFormat :: Maybe ReleaseFormat
                           , _videoContainer :: Maybe AnimeContainer
                           , _animeCodec :: Maybe AnimeCodec
                           , _subtitles :: Subtitles
                           , _resolution :: Maybe Resolution
                           , _audio :: Audio
                           --, dualAudio :: Bool
                           } deriving (Show, Eq)



data MangaInfo = MangaInfo { _scanlated :: Bool
                           , _archived :: Bool
                           , _ongoing :: Bool
                           } deriving (Show, Eq)

data Resolution = Resolution { _width :: Integer
                             , _height :: Integer
                             }
                deriving (Show, Eq)

data ReleaseFormat = DVD | Bluray | TV | Web | VHS | LD
                   deriving (Read, Show, Eq)
data AnimeContainer = MKV | MP4 | ISO | WMV | AVI | VOB | OGM | M2TS
                    deriving (Read, Show, Eq)
data AnimeCodec = H264 | H264HI10P | XviD | DivX | WMV_ | DVD5 | DVD9
                deriving (Read, Show, Eq)
data Subtitles = Hardsub String | Softsub String | RAW | UnknownSubs
               deriving (Show, Eq)

data Audio = MP3 | FLAC | AAC | AC | PCM | DTS String | OtherAudio String
           deriving (Show, Eq)
data MusicCategory = Single | Soundtrack | MusicDVD | Live | EP | RemixCD
                   | PV | LiveAlbum | Compilation | Album | DramaCD
                   deriving (Read, Show, Eq)
data MangaCategory = GenericManga | Manhua | Manhwa | OEL | Oneshot
                   deriving (Read, Show, Eq)
data Category = Anime | Artbook | LiveAction | Game | GameGuide | LightNovel
              | Novel | Anthology | VisualNovel | LiveActionSeries
              | Music MusicCategory | Manga MangaCategory
              deriving (Read, Show, Eq)


type Cost = Integer

data YenPage = YenPage { _yenOwned :: Integer
                       , _spendingLinks :: [(Cost, String)]
                       } deriving Show

makeLenses ''ABTorrent
makeLenses ''ABTorrentGroup
makeLenses ''AnimeInfo
makeLenses ''MangaInfo
makeLenses ''ReleaseFormat
makeLenses ''Resolution
makeLenses ''YenPage
