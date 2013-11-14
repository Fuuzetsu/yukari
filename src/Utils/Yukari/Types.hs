module Utils.Yukari.Types where

import Network.URI


type SiteTitle = String

type InfoSite = (SiteTitle, URI)

data ABTorrent = ABTorrent { torrentID :: Integer
                           , torrentInfoSuffix :: String
                           , torrentInfo :: Information
                           , torrentDownloadURI :: String
                           , torrentURI :: String
                           , torrentSize :: Integer
                           , torrentSnatched :: Integer
                           , torrentSeeders :: Integer
                           , torrentLeechers :: Integer
                           } deriving (Show, Eq)


data ABTorrentGroup = ABTorrentGroup { torrentName :: String
                                     , torrentCategory :: Category
                                     --, torrentSynonyms :: String
                                     --, torrentSnatches :: Integer
                                     --, torrentComments :: Integer
                                     , seriesID :: Integer
                                     , groupID :: Integer
                                     , torrentImageURI :: String
                                     --, torrentMoreInfo :: [InfoSite]
                                     , torrentTags :: [String]
                                     , torrents :: [ABTorrent]
                                     } deriving (Show, Eq)


data Information = AnimeInformation AnimeInfo | MangaInformation MangaInfo | NoInfo deriving (Show, Eq)

data AnimeInfo = AnimeInfo { releaseFormat :: ReleaseFormat
                           , videoContainer :: AnimeContainer
                           , animeCodec :: AnimeCodec
                           , subtitles :: Subtitles
                           , resolution :: Resolution
                           , audio :: Audio
                           --, dualAudio :: Bool
                           } deriving (Show, Eq)

data MangaInfo = MangaInfo { scanlated :: Bool
                           , archived :: Bool
                           , ongoing :: Bool
                           } deriving (Show, Eq)


data ReleaseFormat = DVD | Bluray | TV | Web | VHS | LD deriving (Read, Show, Eq)
data AnimeContainer = MKV | MP4 | ISO | WMV | AVI | VOB | OGM deriving (Read, Show, Eq)
data AnimeCodec = H264 | H264HI10P | XviD | DivX | WMV_ | DVD5 | DVD9 deriving (Read, Show, Eq)
data Subtitles = Hardsub String | Softsub String | RAW | UnknownSubs deriving (Show, Eq)
data Resolution = Resolution Integer Integer deriving (Show, Eq)
data Audio = MP3 | FLAC | AAC | AC | PCM | OtherAudio String deriving (Show, Eq)
data MusicCategory = Single | Soundtrack | MusicDVD | Live | EP | RemixCD
                   | PV | LiveAlbum | Compilation | Album | DramaCD deriving (Read, Show, Eq)
data MangaCategory = GenericManga | Manhua | Manhwa | OEL | Oneshot deriving (Read, Show, Eq)
data Category = Anime | Artbook | LiveAction | Game | GameGuide | LightNovel | Novel | Anthology
              | VisualNovel | LiveActionSeries | Music MusicCategory | Manga MangaCategory deriving (Read, Show, Eq)


type Cost = Integer

data YenPage = YenPage { yenOwned :: Integer
                       , spendingLinks :: [(Cost, String)]
                       } deriving Show
