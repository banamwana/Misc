#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 14 06:54:03 2019

@author: graemepm
"""

import numpy as np
import pandas as pd
from itertools import chain, product
from copy import deepcopy
import time

# =============================================================================
# Solver
# =============================================================================
class Soduko:
    
    def __init__(self, problem):
        
        self.problem = problem
        self.solution = None
        self.grid = None
        self.gridRestoreCopy = None
        self.options = [1,2,3,4,5,6,7,8,9]
        self.remainingOptions = pd.DataFrame([], index=range(9), columns=range(9))
        
    def fill(self):
        
        '''
        Update lists of values for rows, columns, and sub-grids
        '''
        
        if self.grid is None:
            self.grid = deepcopy(self.problem)
            
        self.b00 = [num for num in chain.from_iterable(self.grid[0:3,0:3]) if num != 0]
        self.b01 = [num for num in chain.from_iterable(self.grid[0:3,3:6]) if num != 0]
        self.b02 = [num for num in chain.from_iterable(self.grid[0:3,6:9]) if num != 0]
        self.b10 = [num for num in chain.from_iterable(self.grid[3:6,0:3]) if num != 0]
        self.b11 = [num for num in chain.from_iterable(self.grid[3:6,3:6]) if num != 0]
        self.b12 = [num for num in chain.from_iterable(self.grid[3:6,6:9]) if num != 0]
        self.b20 = [num for num in chain.from_iterable(self.grid[6:9,0:3]) if num != 0]
        self.b21 = [num for num in chain.from_iterable(self.grid[6:9,3:6]) if num != 0]
        self.b22 = [num for num in chain.from_iterable(self.grid[6:9,6:9]) if num != 0]
        
        self.rows = [[num for num in row if num != 0] for row in self.grid]
        self.cols = [[num for num in col if num != 0] for col in self.grid.T]
        
    def narrow(self, data):
        
        '''
        Return remaining options from marginal restrictions
        '''
        
        return [num for num in self.options if num not in data]
    
    def margins(self, cell):
        
        '''
        Combine marginal restrictions of row, col, and box for input cell
        '''
        
        row = cell[0]
        col = cell[1]
        
        rowData = self.rows[row]
        colData = self.cols[col]
        boxData = getattr(self, 'b' + str(int(row/3)) + str(int(col/3)))
    
        return set(rowData + colData + boxData)
    
    def update(self):
        
        '''
        Make dataframe with remaining options for each cell
        '''
        
        self.fill()
        
        for row in range(9):
            for col in range(9):
                
                if self.grid[row,col] in self.options:
                    self.remainingOptions.iat[row,col] = 'com'
                
                if self.grid[row,col] == 0:
                    self.remainingOptions.iat[row, col] = \
                        self.narrow(self.margins((row, col)))
                        
    def insertSolved(self, toInsert):
        
        '''
        Insert values from solved dictionary
        '''
        
        self.gridInsert = deepcopy(self.grid)
        
        for key, value in toInsert.items():
            row, col = key
            self.gridInsert[row, col] = value
            
        self.grid = deepcopy(self.gridInsert)
            
                        
    def solveRows(self):
        
        '''
        Determine exclusive remaining options within row
        '''
        
        toInsert = {}
        
        for number in self.options:
            
            for i, row in self.remainingOptions.iterrows():
                
                found = []
                for j, cell in row.items():
                    if type(cell) is list:
                        if number in cell:
                            found.append((i,j))
                
                if len(found) == 1:
                    toInsert.update({found[0]:number})
        
        self.insertSolved(toInsert)
        self.update()
        
    def solveCols(self):
        
        '''
        Determine exclusive remaining options within column
        '''
        
        toInsert = {}
        
        for number in self.options:
            
            for j, col in self.remainingOptions.iteritems():
                
                found = []
                for i, cell in col.items():
                    if type(cell) is list:
                        if number in cell:
                            found.append((i,j))
                
                if len(found) == 1:
                    toInsert.update({found[0]:number})
                    
        self.insertSolved(toInsert)
        self.update()
        
    def solveBoxes(self):
        
        '''
        Determine exclusive remaining options within sub-grid
        '''
        
        toInsert = {}
        
        for number in self.options:
            
            for i in range(0,9,3):
                for j in range(0,9,3):
                    found = []
                    for x in range(3):
                        for z in range(3):
                            row, col = (i+x, j+z)
                            cell = self.remainingOptions.iloc[row, col]
                            if type(cell) is list:
                                if number in cell:
                                    found.append((row,col))
                                
                    if len(found) == 1:
                        toInsert.update({found[0]:number})
                    
        self.insertSolved(toInsert)
        self.update()
        
    def testSolved(self):
        
        '''
        Check if all rows, columns, and sub-grids are filled in and meet criteria
        '''
        
        rowCheck = np.array([list(set(row)) == self.options for row in self.grid]).all()
        colCheck = np.array([list(set(col)) == self.options for col in self.grid.T]).all()
        
        boxes = []
        for i in range(0,9,3):
            for j in range(0,9,3):
                box = []
                for x in range(3):
                    for z in range(3):
                            box.append(self.grid[i+x, j+z])
                
                boxes.append(box)
        
        boxCheck = np.array([list(set(box)) == self.options for box in boxes]).all()
                
        if np.array([rowCheck, colCheck, boxCheck]).all():
            self.solvedTime = time.time()
            return True
        else:
            return False
        
    def solveSweep(self):
        
        '''
        Solve by filling in unique remaining options across margins
        '''
        
        NewInsert = True
        self.solveSweepCount = 0
        
        while NewInsert and self.solveSweepCount < 50:
            
            self.gridPreSolve = deepcopy(self.grid)
        
            self.solveRows()
            self.solveCols()
            self.solveBoxes()
            self.solveSweepCount += 1
            
            if (self.grid == self.gridPreSolve).all():
                NewInsert = False
    
    def restoreCopy(self):
        
        '''
        Restore grid to state before guess attempt
        '''
        
        self.grid = deepcopy(self.gridRestoreCopy)
        self.update()
        
    def solveGuess(self):
        
        '''
        Solve by guessing rows from possible formulations
        '''
        
        self.gridRestoreCopy = deepcopy(self.grid)
        self.guessAttempts = 0
        
        z1, z2 = np.where(self.grid == 0)
        zDict = {(key1, key2): self.remainingOptions.loc[key1, key2] for key1, key2 in zip(z1, z2)}
        
        for i in range(9):
            
            zRow = [key for key in zDict.keys() if key[0] == i]
            rowOptions = [zDict[key] for key in zRow]
            rowCombs = list(product(*rowOptions))
            rowCombs = [comb for comb in rowCombs if len(set(comb)) == len(comb)]
            
            for comb in rowCombs:
                
                guess = {key: value for key, value in zip(zRow, comb)}
                self.insertSolved(guess)
                self.update()
                self.solveSweep()
                self.guessAttempts += 1
                
                if self.testSolved():
                    return
                else:
                    self.restoreCopy()
                    
                if self.guessAttempts >= 300:
                    return
                    
    def printProblem(self):
        
        '''
        Print problem to console with readable formatting
        '''
        rows = []
        for i in range(9):
            row = ' '.join(self.problem[i,:].astype(str))
            row = row.replace('0', ' ')
            row = '| '.join([row[:6], row[6:12], row[12:]])
            rows.append(row)
        
        line = '---------------------'
        box = rows[:3] + [line] + rows[3:6] + [line] + rows[6:]
        print('Problem:\n')
        print('\n'.join(box))
                    
    def printSolution(self):
        
        '''
        Print solution to console with readable formatting
        '''
        rows = []
        for i in range(9):
            row = ' '.join(self.solution[i,:].astype(str))
            row = '| '.join([row[:6], row[6:12], row[12:]])
            rows.append(row)
        
        line = '---------------------'
        box = rows[:3] + [line] + rows[3:6] + [line] + rows[6:]
        print(str(round(self.solvedTime - self.start,2)) + ' seconds\n')
        print('\n'.join(box))
        
                           
    def solve(self):
        
        '''
        Solve problem
        '''
        
        self.start = time.time()
        
        ## Set up
        self.update()
        
        ## First pass
        self.solveSweep()
        
        if self.testSolved():
            self.solution = self.grid
            self.printProblem()
            print('\nSolved with\n' + str(self.solveSweepCount) + ' sweeps of unique option identifying')
            self.printSolution()
            return
        
        ## Guess entire rows then try sweeping
        self.solveGuess()
        
        if self.testSolved():
            self.solution = self.grid
            self.printProblem()
            print('\nSolved with\n' + str(self.guessAttempts) + ' guess attempts')
            self.printSolution()
            return
        
        else:
            print('\nNo solution found')
            return
               
# =============================================================================
# Problem
# =============================================================================
# np.zeros((9,9), dtype=int)

problem = np.array([
        [0,6,0,0,8,7,0,0,0],
        [0,0,3,9,0,0,6,0,5],
        [0,7,0,0,0,0,0,0,4],
        [6,0,4,8,0,0,0,0,0],
        [0,9,0,0,2,0,0,6,0],
        [0,0,0,0,0,6,1,0,3],
        [1,0,0,0,0,0,0,5,0],
        [2,0,9,0,0,5,4,0,0],
        [0,0,0,2,7,0,0,1,0]
        ])

problem2 = np.array([
        [0,8,0,0,6,0,3,0,7],
        [0,0,0,0,8,0,0,0,0],
        [7,0,1,3,4,0,6,0,9],
        [5,6,2,0,3,7,0,0,0],
        [3,0,0,0,0,0,0,0,5],
        [0,0,0,2,9,0,7,3,6],
        [2,0,5,0,7,8,9,0,4],
        [0,0,0,0,5,0,0,0,0],
        [1,0,8,0,2,0,0,6,0]
        ])
            
problem3 = np.array([
        [4,1,0,7,0,9,0,5,0],
        [3,0,0,0,5,0,1,0,0],
        [0,0,0,0,0,0,0,0,9],
        [0,3,0,0,0,0,7,0,6],
        [0,0,0,2,0,1,0,0,0],
        [8,0,5,0,0,0,0,1,0],
        [9,0,0,0,0,0,0,0,0],
        [0,0,4,0,6,0,0,0,7],
        [0,7,0,3,0,5,0,2,8]
        ])

    
problem4 = np.array([
        [1,0,3,0,0,0,5,0,0],
        [0,0,0,3,0,1,0,7,9],
        [0,4,0,0,5,0,0,0,3],
        [0,3,0,0,0,6,0,0,0],
        [8,0,6,0,2,0,1,0,4],
        [0,0,0,4,0,0,0,6,0],
        [6,0,0,0,4,0,0,5,0],
        [3,2,0,1,0,8,0,0,0],
        [0,0,9,0,0,0,2,0,1]
        ])
    
hardest = np.array([
        [8,0,0,0,0,0,0,0,0],
        [0,0,3,6,0,0,0,0,0],
        [0,7,0,0,9,0,2,0,0],
        [0,5,0,0,0,7,0,0,0],
        [0,0,0,0,4,5,7,0,0],
        [0,0,0,1,0,0,0,3,0],
        [0,0,1,0,0,0,0,6,8],
        [0,0,8,5,0,0,0,1,0],
        [0,0,9,0,0,0,4,0,0]
        ])
    
evil = np.array([[0, 0, 0, 0, 0, 8, 0, 4, 0],
       [4, 5, 0, 0, 0, 7, 6, 0, 0],
       [1, 0, 0, 5, 0, 0, 0, 0, 2],
       [2, 0, 0, 0, 9, 0, 0, 0, 5],
       [0, 8, 0, 0, 0, 0, 0, 3, 0],
       [6, 0, 0, 0, 1, 0, 0, 0, 8],
       [8, 0, 0, 0, 0, 1, 0, 0, 9],
       [0, 0, 6, 3, 0, 0, 0, 2, 7],
       [0, 1, 0, 2, 0, 0, 0, 0, 0]])
    

# =============================================================================
# Solve
# =============================================================================

def main():

    prob = Soduko(hardest)
    prob.solve()

if __name__ == '__main__':
    main()

