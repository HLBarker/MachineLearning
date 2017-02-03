# Author = Hilary Barker
# This script explores the Kaggle "Shelter Animal Outcomes" data and predicts 
# whether the animal was adopted, diea, euthanized, returned to owner, or 
# transfered

# -----------------------------------------------------------------------------
# Load packages 
# -----------------------------------------------------------------------------
library(car)
library(MASS)
library(vegan)

# -----------------------------------------------------------------------------
# Load data 
# -----------------------------------------------------------------------------
test <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/Shelter Animal Outcomes/test.csv")
str(test)

train <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/Shelter Animal Outcomes/train.csv")
str(train)
View(train)


# -----------------------------------------------------------------------------
# Explore data 
# -----------------------------------------------------------------------------
plot(train$AnimalType ~ train$OutcomeType) 
  # Dogs are typically returned to owner
  # Cats more often die in the shelter

plot(train$SexuponOutcome ~ train$OutcomeType) 
  # Spayed/neutered animals are more likely adopted
  # Intact animals are more likely euthanized
levels(train$SexuponOutcome) <- c("NA", "Intact Female", "Intact Male", "Neutered Male", "Spayed Female",
                                  "Unknown")


# make an adult vs. juvenile factor based on "ageuponoutcome"
  # " " = NA
  # 0 - 2 months = Baby
  # 2 - 12 months = Juvenile
  # 1 - 8 year = Adults
  # 8 + year = Senior
levels(train$AgeuponOutcome)
train["AgeClass"] <- train$AgeuponOutcome
levels(train$AgeClass) <- c("NA", "Baby", "Baby", "Baby", "Baby", "Baby", "Juvenile", "Juvenile",
                            "Senior", "Juvenile", "Senior", "Senior", "Senior", "Senior", "Senior",
                            "Senior", "Senior", "Senior", "Senior", "Baby", "Baby", "Baby", "Adult",
                            "Senior", "Baby", "Juvenile", "Baby", "Adult", "Baby", "Juvenile", "Baby",
                            "Adult",  "Baby", "Juvenile", "Baby", "Adult", "Baby", "Juvenile",
                            "Adult", "Juvenile", "Adult", "Juvenile", "Adult", "Juvenile", "Senior")
levels(train$AgeClass)
plot(train$AgeClass ~ train$OutcomeType) 
  # Baby and juvenile pets are most often adopted
  # Baby pets most often die in the shelter
  # Adult pets are most often returned to the owner
  # Baby + Juvenile pets are often transfered


# Need to combine + separate factors in "breed" 
levels(train$Breed)
train["BreedStr"] <- as.character(train$Breed)
train["PrimaryBreed"] <- as.factor(sapply(train$BreedStr, function(x) {strsplit(x, split = '/')[[1]][1]}))
levels(train$PrimaryBreed)
train["PrimaryBreed"] <- as.factor(gsub(" Mix", "", train$PrimaryBreed))
levels(train$PrimaryBreed)

train["SecondaryBreed"] <- as.factor(sapply(train$BreedStr, function(x) {strsplit(x, split = '/')[[1]][2]}))
levels(train$SecondaryBreed)
str(train)
View(train)

# Create a binary variable for typical restricted breeds
  # Pit bulls
  # Staffordshire terriors
  # Rottweilers
  # German shepherds
  # Chow chows
  # Doberman pinschers
  # Akitas
  # Mastiffs
  # Cane corso
  # Great danes
  # Alaskan malamutes
  # Siberian huskies
train["Restricted1"] <- as.numeric(recode(train$PrimaryBreed, "c('Pit Bull','Staffordshire', 'Rottweiler', 'Presa Canario',
                               'German Shepherd', 'Chow Chow', 'Doberman Pinsch', 'Dogue De Bordeaux', 'Neapolitan Mastiff',
                               'Akita', 'Alaskan Malamute', 'American Staffordshire Terrier', 'American Pit Bull Terrier',
                               'Alaskan Husky', 'Cane Corso', 'Mastiff', 'Great Dane', 'Boerboel', 'Burmese', 'Spanish Mastiff',
                               'Siberian Husky', 'Bullmastiff') = '1'; else = '0'"))
train["Restricted2"] <- as.numeric(recode(train$SecondaryBreed, "c('Pit Bull','Staffordshire', 'Rottweiler', 'Presa Canario',
                               'German Shepherd', 'Chow Chow', 'Doberman Pinsch', 'Dogue De Bordeaux', 'Neapolitan Mastiff',
                               'Akita', 'Alaskan Malamute', 'American Staffordshire Terrier', 'American Pit Bull Terrier',
                               'Alaskan Husky', 'Cane Corso', 'Mastiff', 'Great Dane', 'Boerboel', 'Burmese', 'Spanish Mastiff',
                               'Siberian Husky', 'Bullmastiff') = '1'; else = '0'"))
train["RestrictedBreed"] <- as.factor(train$Restricted1 + train$Restricted2)
levels(train$RestrictedBreed) <- c("0", "1", "1")
str(train)
plot(train$RestrictedBreed ~ train$OutcomeType) 
  # Restricted breeds are more often euthanized or returned to owner than
  # transfered or adopted


# create size factor to separate small, medium + large pets
#train["Size"]
train["ExtraLarge"] <- (recode(train$PrimaryBreed, "c('Dogue De Bordeaux', 'Newfoundland', 'Neapolitan Mastiff',
                                          'Great Pyrenees', 'St. Bernard Smooth Coat', 'Leonberger', 'Presa Canario',
                                          'St. Bernard Rough Coat', 'Leonberger', 'Burmese', 'Landseer', 'Spanish Mastiff',
                                          'Cane Corso', 'Mastiff', 'Great Dane', 'Boerboel', 'Irish Wolfhound',
                                          'Bullmastiff', 'Bernese Mountain Dog') = '1'; else = '0'"))
plot(train$ExtraLarge ~ train$OutcomeType) 
  # Very few of these, often returned to owner

train["LargeDog"] <- (recode(train$PrimaryBreed, "c('Afghan Hound', 'Akita', 'Alaskan Husky', 'Alaskan Malamute',
                          'American Pit Bull Terrier', 'American Staffordshire Terrier', 'Anatol Shepherd',
                          'Beauceron', 'Belgian Malinois',  'Black Mouth Cur',  'Bloodhound', 
                          'Bluetick Hound', 'Chesa Bay Retr', 'Chow Chow', 'Dalmatian', 'Doberman Pinsch',
                          'Dogo Argentino', 'Dutch Shepherd', 'English Foxhound', 'English Setter',
                          'English Shepherd', 'Entlebucher', 'Flat Coat Retriever', 'German Shepherd',
                          'Golden Retriever', 'Greyhound', 'Hovawart', 'Kuvasz','Labrador Retriever',
                          'Old English Sheepdog', 'Otterhound', 'Redbone Hound', 'Rhod Ridgeback',
                          'Rottweiler', 'Saluki', 'Schnauzer Giant', 'Siberian Husky', 'Spinone Italiano',
                          'Standard Poodle', 'Swiss Hound') = '1'; else = '0'"))
plot(train$LargeDog ~ train$OutcomeType) 
  # often returned to owner or adopted

train["MediumDog"] <- (recode(train$PrimaryBreed, "c('Airedale Terrier', 'American Bulldog', 'American Foxhound',
                          'Australian Cattle Dog', 'Australian Kelpie', 'Australian Shepherd',
                          'Basset Hound', 'Bearded Collie', 'Belgian Sheepdog', 'Belgian Tervuren',
                          'Blue Lacy', 'Border Collie', 'Boxer', 'Brittany', 'Bull Terrier',
                          'Bulldog', 'Canaan Dog', 'Carolina Dog', 'Catahoula',  'Chinese Sharpei', 
                          'Cocker Spaniel', 'Collie Rough', 'Collie Smooth', 'English Bulldog',
                          'English Cocker Spaniel', 'English Coonhound', 'English Pointer',
                          'English Springer Spaniel', 'Field Spaniel', 'Finnish Spitz', 'French Bulldog',
                          'German Pinscher', 'German Shorthair Pointer', 'German Wirehaired Pointer',
                          'Harrier', 'Ibizan Hound', 'Irish Setter',  'Norwegian Elkhound', 'Nova Scotia Duck Tolling Retriever',
                          'Old English Bulldog', 'Pharaoh Hound', 'Picardy Sheepdog', 'Pit Bull',
                          'Plott Hound', 'Pointer', 'Port Water Dog', 'Queensland Heeler', 'Samoyed',
                          'Shetland Sheepdog', 'Soft Coated Wheaten Terrier', 'Staffordshire',
                          'Standard Schnauzer', 'Treeing Cur', 'Treeing Tennesse Brindle', 'Treeing Walker Coonhound',
                          'Weimaraner', 'Welsh Springer Spaniel', 'Whippet', 'Wirehaired Pointing Griffon') = '1'; else = '0'"))
plot(train$MediumDog ~ train$OutcomeType) 
  # often returned to owner or adopted

train["SmallDog"] <- (recode(train$PrimaryBreed, "c('Affenpinscher', 'American Eskimo', 'Australian Terrier',
                             'Basenji', 'Beagle', 'Bedlington Terr', 'Bichon Frise', 'Border Terrier',
                             'Boston Terrier', 'Boykin Span', 'Bruss Griffon', 'Bull Terrier Miniature',
                             'Cairn Terrier', 'Cardigan Welsh Corgi', 'Cavalier Span', 'Chihuahua Longhair',
                             'Chihuahua Shorthair', 'Chinese Crested', 'Dachshund', 'Dachshund Longhair',
                             'Dachshund Wirehair', 'Feist', 'Glen Of Imaal', 'Havanese', 'Irish Terrier',
                             'Italian Greyhound', 'Jack Russell Terrier', 'Japanese Chin', 'Jindo',
                             'Keeshond', 'Lhasa Apso', 'Lowchen', 'Maltese', 'Manchester Terrier',
                             'Mexican Hairless', 'Miniature Pinscher', 'Miniature Poodle', 
                             'Miniature Schnauzer', 'Norfolk Terrier', 'Norwich Terrier', 'Papillon',
                             'Parson Russell Terrier', 'Patterdale Terr', 'Pbgv', 'Pekingese', 'Pembroke Welsh Corgi',
                             'Podengo Pequeno', 'Pomeranian', 'Pug', 'Rat Terrier', 'Schipperke',
                             'Scottish Terrier', 'Sealyham Terr', 'Shiba Inu', 'Shih Tzu', 'Silky Terrier',
                             'Skye Terrier', 'Smooth Fox Terrier', 'Swedish Vallhund', 'Tibetan Spaniel',
                             'Tibetan Terrier', 'Toy Fox Terrier', 'Toy Poodle', 'Vizsla', 'Welsh Terrier',
                             'West Highland', 'Wire Hair Fox Terrier', 'Yorkshire Terrier') = '1'; else = '0'"))
plot(train$SmallDog ~ train$OutcomeType) 
  # often returned to owner or adopted

train["ShortHairCat"] <- (recode(train$PrimaryBreed, "c('Abyssinian', 'American Shorthair', 'Bengal',
                                 'Bombay', 'British Shorthair', 'Cornish Rex', 'Devon Rex',
                                 'Domestic Shorthair', 'Exotic Shorthair', 'Havana Brown', 'Japanese Bobtail',
                                 'Javanese', 'Manx', 'Ocicat', 'Pixiebob Shorthair', 'Russian Blue',
                                 'Siamese', 'Snowshoe', 'Sphynx', 'Tonkinese', 'Turkish Van') = '1'; else = '0'"))
plot(train$ShortHairCat ~ train$OutcomeType) 
  # often adopted, transfered, or euthanized or died

str(train)

# investigate month and year differences
train$DateTime <- as.character(train$DateTime)
train["Year"] <- as.factor(sapply(train$DateTime, function(x) {strsplit(x, split = '-')[[1]][1]}))
levels(train$Year)
train["Month"] <- as.factor(sapply(train$DateTime, function(x) {strsplit(x, split = '-')[[1]][2]}))
levels(train$Month)
plot(train$Year ~ train$OutcomeType) 


# what's in a name?
train["NameBinary"] <- as.factor(decostand(train$Name, method = "pa"))
plot(train$NameBinary ~ train$OutcomeType) 
  # pets with names are much more likely to be returned to owners

# -----------------------------------------------------------------------------
# Subset data 
# -----------------------------------------------------------------------------
set.seed(20)
train.set <- sample(c(TRUE, FALSE), nrow(train), rep = TRUE)
test.set = (!train.set)
test.y <- train[test.set, ]$OutcomeType


# -----------------------------------------------------------------------------
# LDA models 
# -----------------------------------------------------------------------------
mod1 <- lda(OutcomeType ~ AnimalType + SexuponOutcome + AgeClass, data = train,
            subset = train.set)
lda.pred1 <- predict(mod1, train[test.set, ])
lda.class1 <- lda.pred1$class
table(lda.class1, test.y)
mean(lda.class1 == test.y) # 61.7% correct

mod2 <- lda(OutcomeType ~ AnimalType + SexuponOutcome + AgeClass + ExtraLarge +
              LargeDog + MediumDog + SmallDog, data = train,
            subset = train.set)
lda.pred2 <- predict(mod2, train[test.set, ])
lda.class2 <- lda.pred2$class
table(lda.class2, test.y)
mean(lda.class2 == test.y) # 61.5% correct

mod3 <- lda(OutcomeType ~ AnimalType + SexuponOutcome + AgeClass + RestrictedBreed, data = train,
            subset = train.set)
lda.pred3 <- predict(mod3, train[test.set, ])
lda.class3 <- lda.pred3$class
table(lda.class3, test.y)
mean(lda.class3 == test.y) # 61.6% correct

mod4 <- lda(OutcomeType ~ AnimalType + SexuponOutcome + AgeClass + Year + Month, data = train,
            subset = train.set)
lda.pred4 <- predict(mod4, train[test.set, ])
lda.class4 <- lda.pred4$class
table(lda.class4, test.y)
mean(lda.class4 == test.y) # 61.3% correct


# best model ------------------------------------------------------------------
mod5 <- lda(OutcomeType ~ AnimalType + SexuponOutcome + AgeClass + NameBinary, data = train,
            subset = train.set)
lda.pred5 <- predict(mod5, train[test.set, ])
lda.class5 <- lda.pred5$class
table(lda.class5, test.y)
mean(lda.class5 == test.y) # 62.8% correct
# best model ------------------------------------------------------------------


mod6 <- lda(OutcomeType ~ SexuponOutcome + AgeClass + NameBinary, data = train,
            subset = train.set)
lda.pred6 <- predict(mod6, train[test.set, ])
lda.class6 <- lda.pred6$class
table(lda.class6, test.y)
mean(lda.class6 == test.y) # 62.1% correct


