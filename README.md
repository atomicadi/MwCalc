# Molecular_Weight_Calculator (MwCalc)
![photoart collagemaker picgrid edit photoframe_20253404226362](https://github.com/user-attachments/assets/d4cf1097-e8c2-400d-a35e-3861f0bc1a69)

In this project (written in Fortran), the molecular weight (in amu unit) of a given molecular structure (eg, C6H6) by the user can be easily calculated, by keeping the files (mol_wigt.f90 and Periodic_list.txt) in the same folder. The calculations can be achieved through the terminal, just typing the command:
```sh
gfortran -o mol mol_wigt.f90
```
 where "mol" will be generated as executable file and by running the executable file through the command:
 ```sh
./mol
```
one can easily get the result. \
Also, a default executable file (a.out) will be generated if the executable file name is not mentioned in the command, like:
```sh
gfortran mol_wigt.f90
```
and by using the same command to use the executable, the result can be generated.
So, play with your molecule, hope you will enjoy! \
Though, it has some limitations but trying to resolve that issues and update asap in the version 2.O.
