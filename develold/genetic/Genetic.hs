>Module Genetic where

type populate a= [a]
type select a = [a]->[a]
type mix a = [entity] -> [entity]


data Select entity= Select{ maxpopulationp:: Int
                          , select:: [entity]->[entity]
                          , create::entity
                          , mix:: entity -> entity ->entity
                          , population::[entity]}

class Population entity where
  population :: Select entity

>class Ord entity => GeneticAlgoritm  entity where
     maxpopulation :: Integer
>    populate ::  [entity]             -- create the entities
>    select ::  [entity] -> [entity]   -- order the list of entities
>    mix :: [entity] -> [entity]       -- mix them
>    
>    generation :: Maybe (Population entity) -> (Population entity)
>    generation Nothing= do
>       population <- populate Nothing
>       generation population
>    generation (Just population)= do
>        population2 <- evaluate population
>        mix population2



instance Population entity GeneticAlgoritm entity where
     populate :: take maxpopulation create 
     mix (e:e':es)= take maxpopulation ((mix e e') : mix es)
     select [e]= select e

instance Population entity => Population (Population entity) where
     population = Select{maxpopulation = maxpopulation population entity
                        ,select :: [Population entity]-> Population entity]  -- select between different selection methods for the entity
                        ,...

