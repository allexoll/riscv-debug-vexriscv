.globl main
main:
lui a0, 0
lui a1, 0
lui a2, 0
lui a3, 0
lui a4, 0
lui a5, 0
lui a6, 0
lui a7, 0


start:
   add a0, a0, 1
   add a1, a1, 1
   add a2, a2, 1
   add a3, a3, 1
   add a4, a4, 1
   add a5, a5, 1
   add a6, a6, 1
   add a7, a7, 1
   j pc0
pc0:
   j pc1
pc1:
   j pc2
pc2:
   j pc3
pc3:
   j pc4
pc4:
   j pc5
pc5:
   j pc6
pc6:
   j pc7
pc7:
   j start