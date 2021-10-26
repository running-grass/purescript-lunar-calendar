module Data.Date.Lunar(
  Year,
  Month,
  Day,

  leapDays,
  
  solar2lunar
) where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Date as D
import Data.Enum (toEnum)
import Data.Int (floor)
import Data.Int.Bits (shr, (.&.))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))

type Year = Int
type Month = Int
type Day = Int

type Lyear = Int

type GanZhi = String

type LunarConfig = Int
type MaskCode = Int

type LunarDate = Record (
  year :: Year
  , month :: Month
  , day :: Day
)

minYear :: Year
minYear = 1900

maxYear :: Year
maxYear = 2100

maskLeapDays :: MaskCode
maskLeapDays = 0x10000



validateYear :: Year -> Maybe Year
validateYear year = if year >= minYear && year <= maxYear then Just year else Nothing

validateMonth :: Month -> Maybe Month
validateMonth month = if month >= 1 && month <= 12 then Just month else Nothing

validateDay :: Day -> Maybe Day
validateDay day = if day >= 1 && day <= 31 then Just day else Nothing

_getYearOffset :: Year -> Maybe Int
_getYearOffset year = validateYear year <#> (_ - minYear)

-- 获取年份配置信息
getLunarInfo :: Year -> Maybe LunarConfig
getLunarInfo year = _getYearOffset year >>= A.index lunarInfo

-- 农历1900-2100的润大小信息表
lunarInfo :: Array LunarConfig
lunarInfo =  [0x04bd8,0x04ae0,0x0a570,0x054d5,0x0d260,0x0d950,0x16554,0x056a0,0x09ad0,0x055d2, -- 1900-1909
        0x04ae0,0x0a5b6,0x0a4d0,0x0d250,0x1d255,0x0b540,0x0d6a0,0x0ada2,0x095b0,0x14977, -- 1910-1919
        0x04970,0x0a4b0,0x0b4b5,0x06a50,0x06d40,0x1ab54,0x02b60,0x09570,0x052f2,0x04970, -- 1920-1929
        0x06566,0x0d4a0,0x0ea50,0x16a95,0x05ad0,0x02b60,0x186e3,0x092e0,0x1c8d7,0x0c950, -- 1930-1939
        0x0d4a0,0x1d8a6,0x0b550,0x056a0,0x1a5b4,0x025d0,0x092d0,0x0d2b2,0x0a950,0x0b557, -- 1940-1949
        0x06ca0,0x0b550,0x15355,0x04da0,0x0a5b0,0x14573,0x052b0,0x0a9a8,0x0e950,0x06aa0, -- 1950-1959
        0x0aea6,0x0ab50,0x04b60,0x0aae4,0x0a570,0x05260,0x0f263,0x0d950,0x05b57,0x056a0, -- 1960-1969
        0x096d0,0x04dd5,0x04ad0,0x0a4d0,0x0d4d4,0x0d250,0x0d558,0x0b540,0x0b6a0,0x195a6, -- 1970-1979
        0x095b0,0x049b0,0x0a974,0x0a4b0,0x0b27a,0x06a50,0x06d40,0x0af46,0x0ab60,0x09570, -- 1980-1989
        0x04af5,0x04970,0x064b0,0x074a3,0x0ea50,0x06b58,0x05ac0,0x0ab60,0x096d5,0x092e0, -- 1990-1999
        0x0c960,0x0d954,0x0d4a0,0x0da50,0x07552,0x056a0,0x0abb7,0x025d0,0x092d0,0x0cab5, -- 2000-2009
        0x0a950,0x0b4a0,0x0baa4,0x0ad50,0x055d9,0x04ba0,0x0a5b0,0x15176,0x052b0,0x0a930, -- 2010-2019
        0x07954,0x06aa0,0x0ad50,0x05b52,0x04b60,0x0a6e6,0x0a4e0,0x0d260,0x0ea65,0x0d530, -- 2020-2029
        0x05aa0,0x076a3,0x096d0,0x04afb,0x04ad0,0x0a4d0,0x1d0b6,0x0d250,0x0d520,0x0dd45, -- 2030-2039
        0x0b5a0,0x056d0,0x055b2,0x049b0,0x0a577,0x0a4b0,0x0aa50,0x1b255,0x06d20,0x0ada0, -- 2040-2049
        -- Add By JJonline@JJonline.Cn
        0x14b63,0x09370,0x049f8,0x04970,0x064b0,0x168a6,0x0ea50, 0x06b20,0x1a6c4,0x0aae0, -- 2050-2059
        0x092e0,0x0d2e3,0x0c960,0x0d557,0x0d4a0,0x0da50,0x05d55,0x056a0,0x0a6d0,0x055d4, -- 2060-2069
        0x052d0,0x0a9b8,0x0a950,0x0b4a0,0x0b6a6,0x0ad50,0x055a0,0x0aba4,0x0a5b0,0x052b0, -- 2070-2079
        0x0b273,0x06930,0x07337,0x06aa0,0x0ad50,0x14b55,0x04b60,0x0a570,0x054e4,0x0d160, -- 2080-2089
        0x0e968,0x0d520,0x0daa0,0x16aa6,0x056d0,0x04ae0,0x0a9d4,0x0a2d0,0x0d150,0x0f252, -- 2090-2099
        0x0d520] -- 2100

-- 公历每个月份的天数普通表
solarMonth :: Array Int
solarMonth = [31,28,31,30,31,30,31,31,30,31,30,31]


gan :: Array String
gan = ["甲","乙","丙","丁","戊","己","庚","辛","壬","癸"]

zhi :: Array String
zhi = ["子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"]

animals :: Array String
animals = ["鼠","牛","虎","兔","龙","蛇","马","羊","猴","鸡","狗","猪"]

signs :: Array String
signs = ["魔羯", "水瓶", "双鱼", "白羊", "金牛", "双子", "巨蟹", "狮子", "处女", "天秤", "天蝎", "射手", "魔羯"]

signSplitDay :: Array Day
signSplitDay = [20,19,21,21,21,22,23,23,23,23,22,22]

festival :: Map.Map String String
festival = Map.fromFoldable [
    Tuple "1-1" "元旦节"
    , Tuple "2-14" "情人节"
    , Tuple "5-1" "劳动节"
    , Tuple "5-4" "青年节"
    , Tuple "6-1" "儿童节"
    , Tuple "9-10" "教师节"
    , Tuple "10-1" "国庆节"
    , Tuple "12-25" "圣诞节"

    , Tuple "3-8" "妇女节"
    , Tuple "3-12" "植树节"
    , Tuple "4-1" "愚人节"
    , Tuple "5-12" "护士节"
    , Tuple "7-1" "建党节"
    , Tuple "8-1" "建军节"
    , Tuple "12-24" "平安夜"
]

lfestival :: Map.Map String String
lfestival = Map.fromFoldable [
    Tuple "12-30" "除夕"
    , Tuple "1-1" "春节"
    , Tuple "1-15" "元宵节"   
    , Tuple "2-2" "龙抬头"
    , Tuple "5-5" "端午节"
    , Tuple "7-7" "七夕节"   
    , Tuple "7-15" "中元节"
    , Tuple "8-15" "中秋节"
    , Tuple "9-9" "重阳节"   
    , Tuple "10-1" "寒衣节"
    , Tuple "10-15" "下元节"
    , Tuple "12-8" "腊八节"   
    , Tuple "12-23" "北方小年"
    , Tuple "12-24" "南方小年"
]

solarTerm :: Array String
solarTerm = ["小寒","大寒","立春","雨水","惊蛰","春分","清明","谷雨","立夏","小满","芒种","夏至","小暑","大暑","立秋","处暑","白露","秋分","寒露","霜降","立冬","小雪","大雪","冬至"]

-- 1900-2100各年的24节气日期速查表
sTermInfo :: Array String
sTermInfo = ["9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e","97bcf97c3598082c95f8c965cc920f",
        "97bd0b06bdb0722c965ce1cfcc920f","b027097bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e",
        "97bcf97c359801ec95f8c965cc920f","97bd0b06bdb0722c965ce1cfcc920f","b027097bd097c36b0b6fc9274c91aa",
        "97b6b97bd19801ec9210c965cc920e","97bcf97c359801ec95f8c965cc920f","97bd0b06bdb0722c965ce1cfcc920f",
        "b027097bd097c36b0b6fc9274c91aa","9778397bd19801ec9210c965cc920e","97b6b97bd19801ec95f8c965cc920f",
        "97bd09801d98082c95f8e1cfcc920f","97bd097bd097c36b0b6fc9210c8dc2","9778397bd197c36c9210c9274c91aa",
        "97b6b97bd19801ec95f8c965cc920e","97bd09801d98082c95f8e1cfcc920f","97bd097bd097c36b0b6fc9210c8dc2",
        "9778397bd097c36c9210c9274c91aa","97b6b97bd19801ec95f8c965cc920e","97bcf97c3598082c95f8e1cfcc920f",
        "97bd097bd097c36b0b6fc9210c8dc2","9778397bd097c36c9210c9274c91aa","97b6b97bd19801ec9210c965cc920e",
        "97bcf97c3598082c95f8c965cc920f","97bd097bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b97bd19801ec9210c965cc920e","97bcf97c3598082c95f8c965cc920f","97bd097bd097c35b0b6fc920fb0722",
        "9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e","97bcf97c359801ec95f8c965cc920f",
        "97bd097bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e",
        "97bcf97c359801ec95f8c965cc920f","97bd097bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b97bd19801ec9210c965cc920e","97bcf97c359801ec95f8c965cc920f","97bd097bd07f595b0b6fc920fb0722",
        "9778397bd097c36b0b6fc9210c8dc2","9778397bd19801ec9210c9274c920e","97b6b97bd19801ec95f8c965cc920f",
        "97bd07f5307f595b0b0bc920fb0722","7f0e397bd097c36b0b6fc9210c8dc2","9778397bd097c36c9210c9274c920e",
        "97b6b97bd19801ec95f8c965cc920f","97bd07f5307f595b0b0bc920fb0722","7f0e397bd097c36b0b6fc9210c8dc2",
        "9778397bd097c36c9210c9274c91aa","97b6b97bd19801ec9210c965cc920e","97bd07f1487f595b0b0bc920fb0722",
        "7f0e397bd097c36b0b6fc9210c8dc2","9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e",
        "97bcf7f1487f595b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b97bd19801ec9210c965cc920e","97bcf7f1487f595b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722",
        "9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e","97bcf7f1487f531b0b0bb0b6fb0722",
        "7f0e397bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa","97b6b97bd19801ec9210c965cc920e",
        "97bcf7f1487f531b0b0bb0b6fb0722","7f0e397bd07f595b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b97bd19801ec9210c9274c920e","97bcf7f0e47f531b0b0bb0b6fb0722","7f0e397bd07f595b0b0bc920fb0722",
        "9778397bd097c36b0b6fc9210c91aa","97b6b97bd197c36c9210c9274c920e","97bcf7f0e47f531b0b0bb0b6fb0722",
        "7f0e397bd07f595b0b0bc920fb0722","9778397bd097c36b0b6fc9210c8dc2","9778397bd097c36c9210c9274c920e",
        "97b6b7f0e47f531b0723b0b6fb0722","7f0e37f5307f595b0b0bc920fb0722","7f0e397bd097c36b0b6fc9210c8dc2",
        "9778397bd097c36b0b70c9274c91aa","97b6b7f0e47f531b0723b0b6fb0721","7f0e37f1487f595b0b0bb0b6fb0722",
        "7f0e397bd097c35b0b6fc9210c8dc2","9778397bd097c36b0b6fc9274c91aa","97b6b7f0e47f531b0723b0b6fb0721",
        "7f0e27f1487f595b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722",
        "9778397bd097c36b0b6fc9274c91aa","97b6b7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722",
        "7f0e397bd097c35b0b6fc920fb0722","9778397bd097c36b0b6fc9274c91aa","97b6b7f0e47f531b0723b0b6fb0721",
        "7f0e27f1487f531b0b0bb0b6fb0722","7f0e397bd07f595b0b0bc920fb0722","9778397bd097c36b0b6fc9274c91aa",
        "97b6b7f0e47f531b0723b0787b0721","7f0e27f0e47f531b0b0bb0b6fb0722","7f0e397bd07f595b0b0bc920fb0722",
        "9778397bd097c36b0b6fc9210c91aa","97b6b7f0e47f149b0723b0787b0721","7f0e27f0e47f531b0723b0b6fb0722",
        "7f0e397bd07f595b0b0bc920fb0722","9778397bd097c36b0b6fc9210c8dc2","977837f0e37f149b0723b0787b0721",
        "7f07e7f0e47f531b0723b0b6fb0722","7f0e37f5307f595b0b0bc920fb0722","7f0e397bd097c35b0b6fc9210c8dc2",
        "977837f0e37f14998082b0787b0721","7f07e7f0e47f531b0723b0b6fb0721","7f0e37f1487f595b0b0bb0b6fb0722",
        "7f0e397bd097c35b0b6fc9210c8dc2","977837f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721",
        "7f0e27f1487f531b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722","977837f0e37f14998082b0787b06bd",
        "7f07e7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722","7f0e397bd097c35b0b6fc920fb0722",
        "977837f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722",
        "7f0e397bd07f595b0b0bc920fb0722","977837f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721",
        "7f0e27f1487f531b0b0bb0b6fb0722","7f0e397bd07f595b0b0bc920fb0722","977837f0e37f14998082b0787b06bd",
        "7f07e7f0e47f149b0723b0787b0721","7f0e27f0e47f531b0b0bb0b6fb0722","7f0e397bd07f595b0b0bc920fb0722",
        "977837f0e37f14998082b0723b06bd","7f07e7f0e37f149b0723b0787b0721","7f0e27f0e47f531b0723b0b6fb0722",
        "7f0e397bd07f595b0b0bc920fb0722","977837f0e37f14898082b0723b02d5","7ec967f0e37f14998082b0787b0721",
        "7f07e7f0e47f531b0723b0b6fb0722","7f0e37f1487f595b0b0bb0b6fb0722","7f0e37f0e37f14898082b0723b02d5",
        "7ec967f0e37f14998082b0787b0721","7f07e7f0e47f531b0723b0b6fb0722","7f0e37f1487f531b0b0bb0b6fb0722",
        "7f0e37f0e37f14898082b0723b02d5","7ec967f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721",
        "7f0e37f1487f531b0b0bb0b6fb0722","7f0e37f0e37f14898082b072297c35","7ec967f0e37f14998082b0787b06bd",
        "7f07e7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722","7f0e37f0e37f14898082b072297c35",
        "7ec967f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722",
        "7f0e37f0e366aa89801eb072297c35","7ec967f0e37f14998082b0787b06bd","7f07e7f0e47f149b0723b0787b0721",
        "7f0e27f1487f531b0b0bb0b6fb0722","7f0e37f0e366aa89801eb072297c35","7ec967f0e37f14998082b0723b06bd",
        "7f07e7f0e47f149b0723b0787b0721","7f0e27f0e47f531b0723b0b6fb0722","7f0e37f0e366aa89801eb072297c35",
        "7ec967f0e37f14998082b0723b06bd","7f07e7f0e37f14998083b0787b0721","7f0e27f0e47f531b0723b0b6fb0722",
        "7f0e37f0e366aa89801eb072297c35","7ec967f0e37f14898082b0723b02d5","7f07e7f0e37f14998082b0787b0721",
        "7f07e7f0e47f531b0723b0b6fb0722","7f0e36665b66aa89801e9808297c35","665f67f0e37f14898082b0723b02d5",
        "7ec967f0e37f14998082b0787b0721","7f07e7f0e47f531b0723b0b6fb0722","7f0e36665b66a449801e9808297c35",
        "665f67f0e37f14898082b0723b02d5","7ec967f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721",
        "7f0e36665b66a449801e9808297c35","665f67f0e37f14898082b072297c35","7ec967f0e37f14998082b0787b06bd",
        "7f07e7f0e47f531b0723b0b6fb0721","7f0e26665b66a449801e9808297c35","665f67f0e37f1489801eb072297c35",
        "7ec967f0e37f14998082b0787b06bd","7f07e7f0e47f531b0723b0b6fb0721","7f0e27f1487f531b0b0bb0b6fb0722"]


-- 数字转中文速查表
nStr1 :: Array String
nStr1 = ["日","一","二","三","四","五","六","七","八","九","十"]

-- 日期转农历称呼速查表
nStr2 :: Array String
nStr2 = ["初","十","廿","卅"]

-- 月份转农历称呼速查表
nStr3 :: Array String
nStr3 = ["正","一","二","三","四","五","六","七","八","九","十","冬","腊"]

-- 返回农历y年一整年的总天数
lYearDays :: Year -> Maybe Int
lYearDays year = do
  currLunarInfo <- getLunarInfo year
  let baseDays = 348
  leapDay <- leapDays year
  pure $ (_sum currLunarInfo baseDays 0x8000) + leapDay
  where
    _sum _lunarInfo acc mask  = case mask > 0x8, _lunarInfo .&. mask of
      true, 0 -> _sum _lunarInfo acc $ shr mask 1
      true, _ -> _sum _lunarInfo (acc + 1) $ shr mask 1
      _, _ -> acc


-- 根据年份和掩码获取具体配置信息
isMaskYes :: Year -> MaskCode -> Maybe Boolean
isMaskYes year maskCode = case getLunarInfo year <#> (_ .&. maskCode) of
  Just 0 -> Just false
  Just _ -> Just true
  Nothing -> Nothing


-- 返回农历y年闰月是哪个月；若y年没有闰月 则返回0
leapMonth :: Year -> Maybe Month
leapMonth year = case getLunarInfo year <#> (_  .&. 0xf) of
  Just 0 -> Nothing
  other -> other

-- | 返回农历y年闰月的天数 若该年没有闰月则返回0
leapDays :: Year -> Maybe Int
leapDays year = case leapMonth year, isMaskYes year maskLeapDays of
  Nothing, _ -> Just 0
  Just _, Just true -> Just 30
  Just _, Just false -> Just 29
  _, _ -> Nothing

-- 返回农历y年m月（非闰月）的总天数，计算m为闰月时的天数请使用leapDays方法
monthDays :: Year -> Month -> Maybe Int
monthDays year month = do
  month' <- validateMonth month
  case isMaskYes year (shr maskLeapDays month') of
    Just true -> Just 30
    Just false -> Just 29
    _ -> Nothing


-- 返回公历(!)y年m月的天数
solarDays :: Year -> Month -> Maybe Int
solarDays year month = do
  month' <- validateMonth month
  let ms = month' - 1
  case ms == 1 of
    true -> do
      if (year `mod` 4 == 0) && (year `mod` 100 /= 0) || (year `mod` 400 == 0)
      then Just 29
      else Just 28
    false -> solarMonth !! ms

-- 农历年份转换为干支纪年
toGanZhiYear :: Lyear -> Maybe GanZhi
toGanZhiYear lyear = do
  let ganKey = (lyear - 3) `mod` 10
      zhiKey = (lyear - 3) `mod` 12
      ganKey' = if ganKey == 0 then 10 else ganKey
      zhiKey' = if zhiKey == 0 then 12 else zhiKey
  ganStr <- gan !! ganKey'
  zhiStr <- zhi !! zhiKey'
  Just $ ganStr <> zhiStr

-- 公历月、日判断所属星座
toAstro :: Month -> Day -> Maybe String
toAstro month day = do
  splitD <- signSplitDay !! day
  let subIdx = if day < splitD then month - 1 else 0
  sign <- signs !! subIdx
  Just $ sign <> "座"
  
-- 传入offset偏移量返回干支
toGanZhi :: Int -> Maybe GanZhi
toGanZhi offset = do
  ganStr <- gan !! (offset `mod` 10)
  zhiStr <- zhi !! (offset `mod` 12)
  Just $ ganStr <> zhiStr

-- 传入公历(!)y年获得该年第n个节气的公历日期 TODO
-- getTerm :: Year -> Int -> Int

-- 传入农历数字月份返回汉语通俗表示法
toChinaMonth :: Month -> Maybe String 
toChinaMonth month = do
  month_ <- validateMonth month
  str <- nStr3 !! (month_ - 1)
  Just $ str <> "月"

-- 传入农历日期数字返回汉字表示法
toChinaDay :: Day -> Maybe String
toChinaDay day = do
  case day of
    10 -> Just "初十"
    20 -> Just "二十"
    30 -> Just "三十"
    _ -> do
      fst <- nStr2 !! (day / 10)
      snd <- nStr1 !! (day `mod` 10)
      Just $ fst <> snd

-- 年份转生肖[!仅能大致转换] => 精确划分生肖分界线是“立春” TODO
getAnimal :: Year -> Maybe String
getAnimal year = animals !! ((year - 4) `mod` 12)


-- | 传入阳历年月日获得详细的农历信息 <=>JSON
solar2lunar :: Year -> Month -> Day -> Maybe LunarDate
solar2lunar year month day = do
  year_ <- toEnum year
  month_ <- toEnum month
  day_ <- toEnum day
  date <- D.exactDate year_ month_ day_
  minDay_ <- minDay
  -- let dur = 
  let offset = case D.diff date minDay_ of
              (Days d) -> floor d
  year__ <- calcLYear $ minYear /\ 0 /\ offset
  let currYear' = fst year__
      lastYearDays = fst <<< snd $ year__
      offsetDays' = snd <<< snd $ year__
      currYear = if offsetDays' < 0 then currYear' - 1 else currYear'
      offsetDays = if offsetDays' < 0 then offsetDays' + lastYearDays else offsetDays'

  let leap = fromMaybe (-1) $ (leapMonth currYear)
  month__ <- calcMonth $ 1 /\ 0 /\ offsetDays /\ false /\ leap /\ currYear

  let currMonth = fst month__
      currMonthDays = fst <<< snd $ month__
      currMonthOffset = fst <<< snd <<< snd $ month__
      isLeapMonth = snd <<< snd <<< snd $ month__
      b1 = currMonthOffset == 0 && leap > 0 && currMonth == leap + 1
      isLeap' = if b1 
                then not isLeapMonth
                else isLeapMonth
      currMonth' = if b1 && (not isLeapMonth) then currMonth - 1 else currMonth
      currMonth'' = if currMonthOffset < 0 then currMonth' - 1 else currMonth'
      currMonthOffset' = if currMonthOffset < 0 then currMonthOffset + currMonthDays else currMonthOffset

  let lYear = currYear
      lMonth = currMonth''
      lDay = currMonthOffset' + 1
  --  let offset = coerce dur
  -- guard $ not $ year_ == minYear && month_ == 1 && day_ < 31
  Just { year : lYear, month: lMonth, day: lDay }

  where 
    minDay = do 
      y <- toEnum 1900
      m <- toEnum 1
      d <- toEnum 31
      D.exactDate y m d
    calcLYear :: Tuple Int (Tuple Int Int) -> Maybe (Tuple Int (Tuple Int Int))
    calcLYear tp@(y /\ days /\ offset) = case y < 2101 && offset > 0 of
        false -> Just tp
        true -> do
          nDays <- lYearDays y
          calcLYear $ (y + 1) /\ nDays /\ (offset - nDays)

    calcMonth :: Tuple Int (Tuple Int (Tuple Int (Tuple Boolean (Tuple Int Year)))) -> Maybe (Tuple Int (Tuple Int (Tuple Int Boolean)))
    calcMonth (m /\ days /\ offset /\ isLeap /\ leap /\ y) = case m < 13 && offset > 0 of
      false -> Just $ m /\ days /\ offset /\ isLeap
      true -> do
        let 
            b1 = leap > 0 && m == (leap+1) && isLeap == false
            m' = if b1 then m - 1 else m
            isLeap' = if b1 then true else isLeap
            isLeap'' = if isLeap' == true && m' == (leap+1) then false else isLeap'
        mDays <- if b1 then leapDays y else monthDays y m'

        calcMonth $ (m' + 1) /\ mDays /\ (offset - mDays) /\ isLeap'' /\ leap /\ y