import Data.List

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving(Enum, Show, Eq)

data Month = January | February | March | April | May | June | July | August
            | September | October | November | December
  deriving(Enum, Show, Eq)

loopSuccMonth :: Month -> Month
loopSuccMonth month
  | month == December = January
  | otherwise = succ month

daysInMonth :: Int -> Month -> Int
daysInMonth year month
  | month == September || month == April || month == June || month == November = 30
  | month == February = if isLeapYear year then 29 else 28
  | otherwise = 31

daysInYear :: Int -> Int
daysInYear year 
  | isLeapYear year = 366
  | otherwise = 365

data Date = Date Month Int Int
  deriving(Show, Eq)

getNextDate :: Date -> Date
getNextDate (Date month date year)
  | month == December = Date nextMonth date (year + 1)
  | otherwise = Date nextMonth date year
  where nextMonth = loopSuccMonth month

isLeapYear :: Int -> Bool
isLeapYear year 
  | year `mod` 100 == 0 = year `mod` 400 == 0
  | otherwise = year `mod` 4 == 0

daysSinceMonthStart :: Date -> Integer
daysSinceMonthStart (Date month date year) = fromIntegral (date - 1)

daysSinceYearStart :: Date -> Integer
daysSinceYearStart today@(Date month date year) 
  | month == January = daysMonth
  | otherwise = daysMonth + (fromIntegral . sum . map (daysInMonth year) $ months)
  where months = [(January)..(pred month)]
        daysMonth = daysSinceMonthStart today

daysSinceStart :: Date -> Integer
daysSinceStart today@(Date month date year) 
  | year == 1900 = sinceYearStart
  | otherwise = sinceYearStart + (fromIntegral . sum . map daysInYear $ allYears)
  where allYears = [1900..(year-1)]
        sinceYearStart = daysSinceYearStart today

getDay :: Date -> Day
getDay date  = 
  iterate succ Monday !! fromIntegral (num `mod` 7) 
  where num = daysSinceStart date
        
main = do
  let d = map getDay . takeWhile filterYear  $ (iterate getNextDate startDate)
  let e = filter ((==) Sunday) d 
  print (show . length $ e)
  where filterYear (Date _ _ year) = year < 2001
        startDate = Date January 1 1901
