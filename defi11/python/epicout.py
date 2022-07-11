import random

#Author : Alexis M. alias @epicout - epicout.dev

class rightPrice:

    def __init__(self):
        random.seed(None)
        self.limitHigh = 1000000
        self.limitLow = 1
        self.price = random.randint(self.limitLow, self.limitHigh)

    def guess(self, number):
            if(number > self.price):
                return -1
            elif(number < self.price):
                return 1
            else:
                return 0

    def find(self): # Binary search
        self.lowRange = self.limitLow
        self.maxRange = self.limitHigh + 1

        self.currentGuess = (self.lowRange + self.maxRange) // 2

        self.count = 0

        while(self.guess(self.currentGuess) != 0):

            self.count += 1

            if(self.guess(self.currentGuess) == 1):
                self.lowRange = self.currentGuess

            elif(self.guess(self.currentGuess) == -1):
                self.maxRange = self.currentGuess

            self.currentGuess = (self.lowRange + self.maxRange) // 2

        print("Number found in " + str(self.count) + " iterations [self.price == " + str(self.price) + "] [self.currentGuess == " + str(self.currentGuess) + "]")


tentative = rightPrice().find()
