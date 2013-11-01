
In the attached picture we have walls of different heights. This picture is represented by an array of integers, where the value at each index is the height of the wall. The picture above is represented with an array as [2,5,1,2,3,4,7,7,6]. Now imagine it rains. How much water is going to be accumulated in puddles between walls? We count volume in square blocks of 1X1. So in the attached picture everything to the left of index 1 spills out. Water to the right of index 7 also spills out. We are left with a puddle between 1 and 6 and the volume is 10.

Sample Inputs & Outputs:

{2, 5, 1, 2, 3, 4, 7, 7, 6} == 10
{2, 5, 1, 3, 1, 2, 1, 7, 7, 6} == 17
{2, 3, 1, 2, 3, 1, 3} == 5
{1, 2, 3, 4, 5, 6, 7, 8, 9} == 0
{[9, 8, 7, 6, 5, 4, 3, 2, 1} == 0
{1, 1, 1, 1, 1} == 0
{1, 0, 1} == 1
{5, 0, 5} == 5
{5, 0, 4} == 4
{4, 0, 5} == 4
{4, 0, 5, 0, 2} == 6
{0, 1, 0, 1, 0} == 1
{0, 1, 0, 0, 1, 0} == 2
{4, 2, 2, 1, 1, 1, 3} == 8
{0, 3, 2, 1, 4} == 3
{1, 0, 1, 0} == 1
{1, 0, 1, 2, 0, 2} == 3
{2, 5, 1, 2, 3, 4, 7, 7, 6} == 10
{5, 1, 0, 1} == 1
{2, 5, 1, 2, 3, 4, 7, 7, 6, 3, 5} == 12
{3, 0, 1, 0, 2} == 5

