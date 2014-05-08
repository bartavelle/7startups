{-# LANGUAGE OverloadedStrings #-}
module Startups.CardList where

import Startups.Base
import Startups.Cards

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Monoid

getMaxStage :: CompanyProfile -> CompanyStage
getMaxStage (CompanyProfile Facebook B) = Stage2
getMaxStage (CompanyProfile Microsoft B) = Stage4
getMaxStage _ = Stage3

wondercard :: Company -> CompanyStage -> Cost -> [Effect] -> Card
wondercard c s cost eff = Card ("[" <> T.pack (show c) <> "] " <> phase) 0 Age1 CompanyStage cost [] eff
    where
        phase = case s of
                    Project -> "project phase"
                    Stage1  -> "initial funding"
                    Stage2  -> "grow"
                    Stage3  -> "world takeover"
                    Stage4  -> "And with strange aeons even death may die."

getResourceCard :: CompanyProfile -> CompanyStage -> Card
getResourceCard (CompanyProfile c _) Project = wondercard c Project "" [ProvideResource r 1 Shared]
    where r = case c of
                  Facebook  -> Finance
                  Twitter   -> Youthfulness
                  Apple     -> Vision
                  Google    -> Development
                  Instagram -> Marketing
                  Amazon    -> Adoption
                  Microsoft -> Operations
getResourceCard (CompanyProfile c A) Stage1 = wondercard c Stage1 cost [AddVictory CompanyVictory 3 HappensOnce]
    where
        cost = case c of
                  Facebook  -> "MM"
                  Twitter   -> "OO"
                  Apple     -> "OO"
                  Google    -> "DD"
                  Instagram -> "MM"
                  Amazon    -> "DD"
                  Microsoft -> "OO"
getResourceCard (CompanyProfile c A) Stage2 = wondercard c Stage2 cost eff
    where
        (cost, eff) = case c of
                          Facebook  -> ("DDD" , [Poaching 2])
                          Twitter   -> ("FF"  , [ResourceChoice baseResources Kept])
                          Apple     -> ("MM"  , [GainFunding 9 HappensOnce])
                          Google    -> ("MMM" , [ScientificBreakthrough])
                          Instagram -> ("OO"  , [Opportunity (S.fromList [Age1 .. Age3])])
                          Amazon    -> ("FFF" , [Recycling])
                          Microsoft -> ("MMM" , [AddVictory CompanyVictory 5 HappensOnce])
getResourceCard (CompanyProfile c A) Stage3 = wondercard c Stage3 cost [AddVictory CompanyVictory 7 HappensOnce]
    where
        cost = case c of
                  Facebook  -> "FFFF"
                  Twitter   -> "YY"
                  Apple     -> "VV"
                  Google    -> "DDDD"
                  Instagram -> "FF"
                  Amazon    -> "AA"
                  Microsoft -> "OOOO"
getResourceCard (CompanyProfile c@Facebook  B) s@Stage1 = wondercard c s "OOO"  [Poaching 1, AddVictory CompanyVictory 3 HappensOnce, GainFunding 3 HappensOnce]
getResourceCard (CompanyProfile c@Facebook  B) s@Stage2 = wondercard c s "FFFF" [Poaching 1, AddVictory CompanyVictory 4 HappensOnce, GainFunding 4 HappensOnce]
getResourceCard (CompanyProfile c@Twitter   B) s@Stage1 = wondercard c s "DD"   [ResourceChoice baseResources Kept]
getResourceCard (CompanyProfile c@Twitter   B) s@Stage2 = wondercard c s "MM"   [ResourceChoice advancedResources Kept]
getResourceCard (CompanyProfile c@Twitter   B) s@Stage3 = wondercard c s "OOO"  [AddVictory CompanyVictory 7 HappensOnce]
getResourceCard (CompanyProfile c@Apple     B) s@Stage1 = wondercard c s "OO"   [AddVictory CompanyVictory 2 HappensOnce, GainFunding 4 HappensOnce]
getResourceCard (CompanyProfile c@Apple     B) s@Stage2 = wondercard c s "MM"   [AddVictory CompanyVictory 3 HappensOnce, GainFunding 4 HappensOnce]
getResourceCard (CompanyProfile c@Apple     B) s@Stage3 = wondercard c s "YVA"  [AddVictory CompanyVictory 5 HappensOnce, GainFunding 4 HappensOnce]
getResourceCard (CompanyProfile c@Google    B) s@Stage1 = wondercard c s "DA"   [AddVictory CompanyVictory 3 HappensOnce]
getResourceCard (CompanyProfile c@Google    B) s@Stage2 = wondercard c s "MMY"  [Efficiency]
getResourceCard (CompanyProfile c@Google    B) s@Stage3 = wondercard c s "DDDV" [ScientificBreakthrough]
getResourceCard (CompanyProfile c@Instagram B) s@Stage1 = wondercard c s "MM"   [CheapExchange baseResources (S.fromList [NLeft, NRight])]
getResourceCard (CompanyProfile c@Instagram B) s@Stage2 = wondercard c s "OO"   [AddVictory CompanyVictory 5 HappensOnce]
getResourceCard (CompanyProfile c@Instagram B) s@Stage3 = wondercard c s "FFA"  [CopyCommunity]
getResourceCard (CompanyProfile c@Amazon    B) s@Stage1 = wondercard c s "FF"   [AddVictory CompanyVictory 2 HappensOnce, Recycling]
getResourceCard (CompanyProfile c@Amazon    B) s@Stage2 = wondercard c s "DDD"  [AddVictory CompanyVictory 1 HappensOnce, Recycling]
getResourceCard (CompanyProfile c@Amazon    B) s@Stage3 = wondercard c s "YVA"  [Recycling]
getResourceCard (CompanyProfile c@Microsoft B) s@Stage1 = wondercard c s "MM"   [AddVictory CompanyVictory 3 HappensOnce]
getResourceCard (CompanyProfile c@Microsoft B) s@Stage2 = wondercard c s "OOO"  [AddVictory CompanyVictory 5 HappensOnce]
getResourceCard (CompanyProfile c@Microsoft B) s@Stage3 = wondercard c s "DDD"  [AddVictory CompanyVictory 5 HappensOnce]
getResourceCard (CompanyProfile c@Microsoft B) s@Stage4 = wondercard c s "OOOOA"[AddVictory CompanyVictory 7 HappensOnce]
getResourceCard _ _ = error "Invalid card"

communities :: [Card]
communities = [ community "Workers Union"         "FFDOM" [perCard 1 neighbors [BaseResource]]
              , community "Hipster Bar"           "FFOO"  [perCard 2 neighbors [AdvancedResource]]
              , community "Caviar Restaurant"     "YVA"   [perCard 1 neighbors [Commercial]]
              , community "Train Club"            "DDDAV" [perCard 1 neighbors [ResearchDevelopment]]
              , community "No-Poaching Agreement" "DDDY"  [perCard 1 neighbors [HeadHunting]]
              , community "Gloating Party"        "FFOA"  [AddVictory CommunityVictory 1 (ByPoachingResult neighbors (S.singleton Defeat))]
              , community "Secret Society"        "MMMVY" [perCard 1 myself [BaseResource,AdvancedResource,Community]]
              , community "Science Club"          "MMFFV" [ScientificBreakthrough]
              , community "Legal Department"      "MMMOV" [perCard 1 neighbors [Commercial]]
              , community "Company Monument"      "OODDY" [AddVictory CommunityVictory 1 (ByStartupStage everyone)]
              ]

allcards :: [Card]
allcards = [ Card "Marketroid"            3 Age1 BaseResource ""  [] [ProvideResource Marketing 1 Shared]
           , Card "Marketroid"            4 Age1 BaseResource ""  [] [ProvideResource Marketing 1 Shared]
           , Card "IT Technician"         3 Age1 BaseResource ""  [] [ProvideResource Operations 1 Shared]
           , Card "IT Technician"         5 Age1 BaseResource ""  [] [ProvideResource Operations 1 Shared]
           , Card "Enterprise Programmer" 3 Age1 BaseResource ""  [] [ProvideResource Development 1 Shared]
           , Card "Enterprise Programmer" 5 Age1 BaseResource ""  [] [ProvideResource Development 1 Shared]
           , Card "Accountant"            3 Age1 BaseResource ""  [] [ProvideResource Finance 1 Shared]
           , Card "Accountant"            4 Age1 BaseResource ""  [] [ProvideResource Finance 1 Shared]
           , Card "Feature Driven Team"   6 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Marketing, Development]) Shared]
           , Card "Operations Guru"       3 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Marketing, Operations]) Shared]
           , Card "Value Optimizer"       5 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Marketing, Finance]) Shared]
           , Card "Devops Team"           4 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Operations, Development]) Shared]
           , Card "Financial Developer"   3 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Development, Finance]) Shared]
           , Card "High Frequency Trader" 6 Age1 BaseResource "$" [] [ResourceChoice (S.fromList [Finance, Operations]) Shared]
           , Card "Marketing Expert"      3 Age2 BaseResource "$" [] [ProvideResource Marketing 2 Shared]
           , Card "Marketing Expert"      4 Age2 BaseResource "$" [] [ProvideResource Marketing 2 Shared]
           , Card "IT Architect"          3 Age2 BaseResource "$" [] [ProvideResource Operations 2 Shared]
           , Card "IT Architect"          4 Age2 BaseResource "$" [] [ProvideResource Operations 2 Shared]
           , Card "Functional Programmer" 3 Age2 BaseResource "$" [] [ProvideResource Development 2 Shared]
           , Card "Functional Programmer" 4 Age2 BaseResource "$" [] [ProvideResource Development 2 Shared]
           , Card "Double Irish Expert"   3 Age2 BaseResource "$" [] [ProvideResource Finance 2 Shared]
           , Card "Double Irish Expert"   4 Age2 BaseResource "$" [] [ProvideResource Finance 2 Shared]

           , Card "Rock Star Evangelist"  3 Age1 AdvancedResource "" [] [ProvideResource Adoption 1 Shared]
           , Card "Rock Star Evangelist"  6 Age1 AdvancedResource "" [] [ProvideResource Adoption 1 Shared]
           , Card "Rock Star Evangelist"  3 Age2 AdvancedResource "" [] [ProvideResource Adoption 1 Shared]
           , Card "Rock Star Evangelist"  5 Age2 AdvancedResource "" [] [ProvideResource Adoption 1 Shared]
           , Card "Company Nerf Battles"  3 Age1 AdvancedResource "" [] [ProvideResource Youthfulness 1 Shared]
           , Card "Company Nerf Battles"  6 Age1 AdvancedResource "" [] [ProvideResource Youthfulness 1 Shared]
           , Card "Company Nerf Battles"  3 Age2 AdvancedResource "" [] [ProvideResource Youthfulness 1 Shared]
           , Card "Company Nerf Battles"  5 Age2 AdvancedResource "" [] [ProvideResource Youthfulness 1 Shared]
           , Card "Charismatic Leader"    3 Age1 AdvancedResource "" [] [ProvideResource Vision 1 Shared]
           , Card "Charismatic Leader"    6 Age1 AdvancedResource "" [] [ProvideResource Vision 1 Shared]
           , Card "Charismatic Leader"    3 Age2 AdvancedResource "" [] [ProvideResource Vision 1 Shared]
           , Card "Charismatic Leader"    5 Age2 AdvancedResource "" [] [ProvideResource Vision 1 Shared]

           , Card "High Speed Internet"    4 Age1 Infrastructure ""        []                        [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "High Speed Internet"    7 Age1 Infrastructure ""        []                        [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "Admin Network"          3 Age1 Infrastructure "O"       ["Operations Center"]     [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "Admin Network"          7 Age1 Infrastructure "O"       ["Operations Center"]     [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "Operations Center"      3 Age2 Infrastructure "OOO"     []                        [AddVictory InfrastructureVictory 5 HappensOnce]
           , Card "Operations Center"      7 Age2 Infrastructure "OOO"     []                        [AddVictory InfrastructureVictory 5 HappensOnce]
           , Card "Cloud Servers"          3 Age1 Infrastructure ""        ["Collocated Datacenter"] [AddVictory InfrastructureVictory 2 HappensOnce]
           , Card "Cloud Servers"          5 Age1 Infrastructure ""        ["Collocated Datacenter"] [AddVictory InfrastructureVictory 2 HappensOnce]
           , Card "Collocated Datacenter"  3 Age2 Infrastructure "MDY"     ["Company Datacenter"]    [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "Collocated Datacenter"  6 Age2 Infrastructure "MDY"     ["Company Datacenter"]    [AddVictory InfrastructureVictory 3 HappensOnce]
           , Card "Company Datacenter"     3 Age3 Infrastructure "DDFYVA"  []                        [AddVictory InfrastructureVictory 7 HappensOnce]
           , Card "Company Datacenter"     6 Age3 Infrastructure "DDFYVA"  []                        [AddVictory InfrastructureVictory 7 HappensOnce]
           , Card "Garage"                 3 Age1 Infrastructure ""        ["Office"]                [AddVictory InfrastructureVictory 2 HappensOnce]
           , Card "Garage"                 6 Age1 Infrastructure ""        ["Office"]                [AddVictory InfrastructureVictory 2 HappensOnce]
           , Card "Office"                 3 Age2 Infrastructure "FFM"     ["Company Building"]      [AddVictory InfrastructureVictory 4 HappensOnce]
           , Card "Office"                 7 Age2 Infrastructure "FFM"     ["Company Building"]      [AddVictory InfrastructureVictory 4 HappensOnce]
           , Card "Company Building"       3 Age3 Infrastructure "DDM"     []                        [AddVictory InfrastructureVictory 5 HappensOnce]
           , Card "Company Building"       4 Age3 Infrastructure "DDM"     []                        [AddVictory InfrastructureVictory 5 HappensOnce]
           , Card "Custom Routers"         3 Age2 Infrastructure "DDA"     []                        [AddVictory InfrastructureVictory 4 HappensOnce]
           , Card "Custom Routers"         5 Age2 Infrastructure "DDA"     []                        [AddVictory InfrastructureVictory 4 HappensOnce]
           , Card "Custom Servers"         3 Age3 Infrastructure "MMFO"    []                        [AddVictory InfrastructureVictory 6 HappensOnce]
           , Card "Custom Servers"         5 Age3 Infrastructure "MMFO"    []                        [AddVictory InfrastructureVictory 6 HappensOnce]
           , Card "National Fiber Network" 3 Age3 Infrastructure "OOFY"    []                        [AddVictory InfrastructureVictory 6 HappensOnce]
           , Card "National Fiber Network" 5 Age3 Infrastructure "OOFY"    []                        [AddVictory InfrastructureVictory 6 HappensOnce]
           , Card "National Fiber Network" 6 Age3 Infrastructure "OOFY"    []                        [AddVictory InfrastructureVictory 6 HappensOnce]
           , Card "Lavish Headquarters"    3 Age3 Infrastructure "YVADOMF" []                        [AddVictory InfrastructureVictory 8 HappensOnce]
           , Card "Lavish Headquarters"    7 Age3 Infrastructure "YVADOMF" []                        [AddVictory InfrastructureVictory 8 HappensOnce]

           , Card "Free Drinks"            3 Age1 HeadHunting "M"    []                    [Poaching 1]
           , Card "Free Drinks"            7 Age1 HeadHunting "M"    []                    [Poaching 1]
           , Card "Standing Desks"         3 Age1 HeadHunting "F"    []                    [Poaching 1]
           , Card "Standing Desks"         5 Age1 HeadHunting "F"    []                    [Poaching 1]
           , Card "Free Food"              3 Age1 HeadHunting "D"    []                    [Poaching 1]
           , Card "Free Food"              4 Age1 HeadHunting "D"    []                    [Poaching 1]
           , Card "Car Fleet"              3 Age2 HeadHunting "OOO"  ["Segways"]           [Poaching 2]
           , Card "Car Fleet"              7 Age2 HeadHunting "OOO"  ["Segways"]           [Poaching 2]
           , Card "Segways"                3 Age3 HeadHunting "FFFO" []                    [Poaching 3]
           , Card "Segways"                7 Age3 HeadHunting "FFFO" []                    [Poaching 3]
           , Card "Nap Rooms"              4 Age2 HeadHunting "FFM"  ["High-Tech Toilets"] [Poaching 2]
           , Card "Nap Rooms"              6 Age2 HeadHunting "FFM"  ["High-Tech Toilets"] [Poaching 2]
           , Card "Nap Rooms"              7 Age2 HeadHunting "FFM"  ["High-Tech Toilets"] [Poaching 2]
           , Card "High-Tech Toilets"      4 Age3 HeadHunting "OOOF" []                    [Poaching 3]
           , Card "High-Tech Toilets"      5 Age3 HeadHunting "OOOF" []                    [Poaching 3]
           , Card "High-Tech Toilets"      6 Age3 HeadHunting "OOOF" []                    [Poaching 3]
           , Card "Ball Pits"              3 Age3 HeadHunting "FMMA" []                    [Poaching 3]
           , Card "Ball Pits"              4 Age3 HeadHunting "FMMA" []                    [Poaching 3]
           , Card "Ball Pits"              7 Age3 HeadHunting "FMMA" []                    [Poaching 3]
           , Card "Technology Guru"        3 Age2 HeadHunting "FDM"  []                    [Poaching 2]
           , Card "Technology Guru"        5 Age2 HeadHunting "FDM"  []                    [Poaching 2]
           , Card "Cool Internal Language" 3 Age2 HeadHunting "MMF"  []                    [Poaching 2]
           , Card "Cool Internal Language" 6 Age2 HeadHunting "MMF"  []                    [Poaching 2]
           , Card "Can Work In Haskell"    3 Age3 HeadHunting "MDDD" []                    [Poaching 3]
           , Card "Can Work In Haskell"    5 Age3 HeadHunting "MDDD" []                    [Poaching 3]

           , Card "Database Sharding"        3 Age1 ResearchDevelopment "A"    ["Global Databases", "Technology Guru"]       [RnD Scaling]
           , Card "Database Sharding"        5 Age1 ResearchDevelopment "A"    ["Global Databases", "Technology Guru"]       [RnD Scaling]
           , Card "Global Databases"         3 Age2 ResearchDevelopment "FFY"  ["Tech Conference", "Architecture Mastery"]   [RnD Scaling]
           , Card "Global Databases"         4 Age2 ResearchDevelopment "FFY"  ["Tech Conference", "Architecture Mastery"]   [RnD Scaling]
           , Card "Architecture Mastery"     3 Age3 ResearchDevelopment "DDVA" []                                            [RnD Scaling]
           , Card "Architecture Mastery"     6 Age3 ResearchDevelopment "DDVA" []                                            [RnD Scaling]
           , Card "Anything But Java"        3 Age1 ResearchDevelopment "Y"    ["Proper Typing", "Cool Internal Language"]   [RnD Programming]
           , Card "Anything But Java"        7 Age1 ResearchDevelopment "Y"    ["Proper Typing", "Cool Internal Language"]   [RnD Programming]
           , Card "Proper Typing"            3 Age2 ResearchDevelopment "DDV"  ["Functional Mastery", "Can Work In Haskell"] [RnD Programming]
           , Card "Proper Typing"            5 Age2 ResearchDevelopment "DDV"  ["Functional Mastery", "Can Work In Haskell"] [RnD Programming]
           , Card "Functional Mastery"       3 Age3 ResearchDevelopment "FFYA" []                                            [RnD Programming]
           , Card "Functional Mastery"       7 Age3 ResearchDevelopment "FFYA" []                                            [RnD Programming]
           , Card "Hardware Knowledge"       3 Age1 ResearchDevelopment "V"    ["Custom Routers", "Efficiency Advances"]     [RnD CustomSolution]
           , Card "Hardware Knowledge"       4 Age1 ResearchDevelopment "V"    ["Custom Routers", "Efficiency Advances"]     [RnD CustomSolution]
           , Card "Efficiency Advances"      3 Age2 ResearchDevelopment "OOA"  ["Custom Servers", "Seven Nines"]             [RnD CustomSolution]
           , Card "Efficiency Advances"      6 Age2 ResearchDevelopment "OOA"  ["Custom Servers", "Seven Nines"]             [RnD CustomSolution]
           , Card "Seven Nines"              3 Age3 ResearchDevelopment "MMYV" []                                            [RnD CustomSolution]
           , Card "Seven Nines"              4 Age3 ResearchDevelopment "MMYV" []                                            [RnD CustomSolution]
           , Card "Efficient Fiber Networks" 3 Age2 ResearchDevelopment "MV"   ["Global Clusters","Cloud Language"]          [RnD CustomSolution]
           , Card "Efficient Fiber Networks" 7 Age2 ResearchDevelopment "MV"   ["Global Clusters","Cloud Language"]          [RnD CustomSolution]
           , Card "Global Clusters"          3 Age3 ResearchDevelopment "OOOY" []                                            [RnD Scaling]
           , Card "Global Clusters"          7 Age3 ResearchDevelopment "OOOY" []                                            [RnD Scaling]
           , Card "Cloud Language"           3 Age3 ResearchDevelopment "MAV"  []                                            [RnD Programming]
           , Card "Cloud Language"           5 Age3 ResearchDevelopment "MAV"  []                                            [RnD Programming]

           , Card "Business Angel"    4 Age1 Commercial ""    []                    [GainFunding 5 HappensOnce]
           , Card "Business Angel"    5 Age1 Commercial ""    []                    [GainFunding 5 HappensOnce]
           , Card "Business Angel"    7 Age1 Commercial ""    []                    [GainFunding 5 HappensOnce]
           , Card "Right Partnership" 3 Age1 Commercial "$"   ["Think Tank"]        [CheapExchange baseResources (S.singleton NRight)]
           , Card "Right Partnership" 7 Age1 Commercial "$"   ["Think Tank"]        [CheapExchange baseResources (S.singleton NRight)]
           , Card "Left Partnership"  3 Age1 Commercial "$"   ["Think Tank"]        [CheapExchange baseResources (S.singleton NLeft)]
           , Card "Left Partnership"  7 Age1 Commercial "$"   ["Think Tank"]        [CheapExchange baseResources (S.singleton NLeft)]
           , Card "Corporate Spy"     3 Age1 Commercial "$"   ["Offshore Labor"]    [CheapExchange advancedResources (S.fromList [NLeft, NRight])]
           , Card "Corporate Spy"     6 Age1 Commercial "$"   ["Offshore Labor"]    [CheapExchange advancedResources (S.fromList [NLeft, NRight])]
           , Card "Think Tank"        3 Age2 Commercial "DD"  ["Software Product"]  [ResourceChoice advancedResources Kept]
           , Card "Think Tank"        6 Age2 Commercial "DD"  ["Software Product"]  [ResourceChoice advancedResources Kept]
           , Card "Think Tank"        7 Age2 Commercial "DD"  ["Software Product"]  [ResourceChoice advancedResources Kept]
           , Card "Offshore Labor"    3 Age2 Commercial "MM"  ["Brand Recognition"] [ResourceChoice baseResources Kept]
           , Card "Offshore Labor"    5 Age2 Commercial "MM"  ["Brand Recognition"] [ResourceChoice baseResources Kept]
           , Card "Offshore Labor"    6 Age2 Commercial "MM"  ["Brand Recognition"] [ResourceChoice baseResources Kept]
           , Card "Development Gig"   3 Age2 Commercial ""    []                    [GainFunding 1 (PerCard everyone (S.singleton BaseResource))]
           , Card "Development Gig"   6 Age2 Commercial ""    []                    [GainFunding 1 (PerCard everyone (S.singleton BaseResource))]
           , Card "Consulting Gig"    4 Age2 Commercial ""    []                    [GainFunding 2 (PerCard everyone (S.singleton AdvancedResource))]
           , Card "Consulting Gig"    7 Age2 Commercial ""    []                    [GainFunding 2 (PerCard everyone (S.singleton AdvancedResource))]
           , Card "Software Product"  3 Age3 Commercial "MFA" []                    [GainFunding 1 (PerCard myself (S.singleton BaseResource)), perCard 1 myself [BaseResource]]
           , Card "Software Product"  4 Age3 Commercial "MFA" []                    [GainFunding 1 (PerCard myself (S.singleton BaseResource)), perCard 1 myself [BaseResource]]
           , Card "Brand Recognition" 3 Age3 Commercial "OY"  []                    [GainFunding 1 (PerCard myself (S.singleton Commercial)), perCard 1 myself [Commercial]]
           , Card "Brand Recognition" 6 Age3 Commercial "OY"  []                    [GainFunding 1 (PerCard myself (S.singleton Commercial)), perCard 1 myself [Commercial]]
           , Card "Global Vision"     4 Age3 Commercial "DDV" []                    [GainFunding 2 (PerCard myself (S.singleton AdvancedResource)), perCard 2 myself [AdvancedResource]]
           , Card "Global Vision"     6 Age3 Commercial "DDV" []                    [GainFunding 2 (PerCard myself (S.singleton AdvancedResource)), perCard 2 myself [AdvancedResource]]
           , Card "Tech Conference"   3 Age3 Commercial "OOF" []                    [GainFunding 3 (ByStartupStage myself), AddVictory CommercialVictory 1 (ByStartupStage myself)]
           , Card "Tech Conference"   5 Age3 Commercial "OOF" []                    [GainFunding 3 (ByStartupStage myself), AddVictory CommercialVictory 1 (ByStartupStage myself)]
           , Card "Tech Conference"   7 Age3 Commercial "OOF" []                    [GainFunding 3 (ByStartupStage myself), AddVictory CommercialVictory 1 (ByStartupStage myself)]

           ]

perCard :: VictoryPoint -> Target -> [CardType] -> Effect
perCard v t ct = AddVictory CommunityVictory v (PerCard t (S.fromList ct))

community :: T.Text -> Cost -> [Effect] -> Card
community desc c eff = Card desc 0 Age3 Community c [] eff

