! Aditya Barman, Project Associate, MNIT Jaipur, Rajasthan, India; March 02, 2025 
! Email: atomicadi2023@gmail.com

program molecular_weight_calculator
       implicit none
       integer :: i, j, mol_len, num_int, mol_num_int 
          real :: sum, val, sum_final 
          real, allocatable :: atom_mass(:)
       character(len=20) :: mol_string, num_list, mol_num   
       character(len=2)  :: atom_list 
       character(len=2), allocatable :: atom_symbl(:)
    
       print *, "Put your molecular formula (e.g. C2H4/HBr/2H2O)"
       read (*,*) mol_string
       mol_len = len_trim(mol_string)

       allocate(atom_symbl(110), atom_mass(110))
       open(unit=10, file= "Periodic_list.txt")
       do i = 1, 110
         read(10, '(a2,6x,F8.3)') atom_symbl(i), atom_mass(i)
       end do

        sum = 0.0
        val = 0.0 
          i = 1
          j = 1
             
         do  while (i <= mol_len)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! For double apbhabetical symbols (e.g. Cl, Na etc.)

               if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                   (mol_string(i+1:i+1) >= 'a' .and. mol_string(i+1:i+1) <= 'z') .and. &
                   (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                   (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9') .and. &
                   (mol_string(i+4:i+4) >= '1' .and. mol_string(i+4:i+4) <= '9') .and. &
                   (mol_string(i+5:i+5) >= '1' .and. mol_string(i+5:i+5) <= '9')) then
                 go to 200
                 

  
              else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                       (mol_string(i+1:i+1) >= 'a' .and. mol_string(i+1:i+1) <= 'z') .and. &
                       (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                       (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9') .and. &
                       (mol_string(i+4:i+4) >= '1' .and. mol_string(i+4:i+4) <= '9')) then 
               atom_list = mol_string(i:i+1)
               num_list = mol_string(i+2:i+4)
               call str2int(num_list,num_int)
               do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0    
               i = i + 5

               else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                        (mol_string(i+1:i+1) >= 'a' .and. mol_string(i+1:i+1) <= 'z') .and. &
                        (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                        (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9')) then 
               atom_list = mol_string(i:i+1)
               num_list = mol_string(i+2:i+3)
               call str2int(num_list,num_int)
               do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0   
               i = i + 4

                else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                         (mol_string(i+1:i+1) >= 'a' .and. mol_string(i+1:i+1) <= 'z') .and. &
                         (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9')) then 
               atom_list = mol_string(i:i+1)
               num_list = mol_string(i+2:i+2)
               call str2int(num_list,num_int)
               do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0    
               i = i + 3

                else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                         (mol_string(i+1:i+1) >= 'a' .and. mol_string(i+1:i+1) <= 'z')) then 
               atom_list = mol_string(i:i+1)
               do  while (j <= 110)
                   if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * 1.0)
                    j = j + 110
                   else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0   
               i = i + 2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!For single apbhabetical symbols (e.g. H, O, N etc.)

                else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                         (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                         (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                         (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9') .and. &
                         (mol_string(i+4:i+4) >= '1' .and. mol_string(i+4:i+4) <= '9')) then
                sum_final = 0.0
               go to 200
                
 
               else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                        (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                        (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                        (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9')) then 
               atom_list = mol_string(i:i)
               num_list = mol_string(i+1:i+3)
               call str2int(num_list,num_int)
               do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0   
               i = i + 4

              else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                       (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                       (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9')) then 
               atom_list = mol_string(i:i)
               num_list = mol_string(i+1:i+2)
               call str2int(num_list,num_int)
               do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                   go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0   
               i = i + 3

              else if ((mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') .and. &
                       (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9')) then 
               atom_list = mol_string(i:i)
               num_list = mol_string(i+1:i+1)
               call str2int(num_list,num_int)
                do  while (j <= 110)
                  if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * num_int)
                    j = j + 110
                  else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0  
                 i = i + 2

               else if (mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') then 
                atom_list = mol_string(i:i)
                 do  while (j <= 110)
                   if (trim(atom_list) == trim(atom_symbl(j))) then
                    val = val + (atom_mass(j) * 1.0)
                    j = j + 110
                   else 
                      val = 0.0 + val
                      j = j + 1
                  end if
                end do
                   j = 1
                 if (val == 0.0) then
                    go to 100
                 else
                     sum = sum + val
                 end if
                 val = 0.0   
               i = i + 1

             else 
                i = i + 1
                
            end if
           end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!For intial numbers (e.g. 2H2O, 2Na etc.)
   
       i = 1
       do while (i <= mol_len)
           if (mol_string(i:i) >= 'A' .and. mol_string(i:i) <= 'Z') then
           mol_num_int = 1
           exit

           else if ((mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') .and. &
                    (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                    (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9') .and. &
                    (mol_string(i+3:i+3) >= '1' .and. mol_string(i+3:i+3) <= '9')) then
             sum_final = 0.0
             go to 200
             

           else if ((mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') .and. &
                    (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                    (mol_string(i+2:i+2) >= '1' .and. mol_string(i+2:i+2) <= '9')) then
            mol_num = mol_string(i:i+2)
            call str2int(mol_num,mol_num_int)
            mol_num_int = mol_num_int
            exit 
         
           else if ((mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') .and. &
                    (mol_string(i+1:i+1) >= '1' .and. mol_string(i+1:i+1) <= '9') .and. &
                    (mol_string(i+2:i+2) >= 'A' .and. mol_string(i+2:i+2) <= 'Z'))  then
            mol_num = mol_string(i:i+1)
            call str2int(mol_num,mol_num_int)
            mol_num_int = mol_num_int
            exit

            else if ((mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') .and. &
                     (mol_string(i+1:i+1) >= 'A' .and. mol_string(i+1:i+1) <= 'Z')) then
            mol_num = mol_string(i:i)
            call str2int(mol_num,mol_num_int)
            mol_num_int = mol_num_int
            exit

            else 
               go to 200
              exit 
          end if
        end do
    
          sum_final = mol_num_int * sum 
          if (sum_final /= 0.0) then 
             go to 300
          end if
           
  100     print *, "Error: Some of the Atoms are not listed in the Periodic_list, Sorry!"
          stop
  200     print *, "Error: Out of capacity, Sorry!"
          stop
  300     print *, "Molecular weight of ",  trim(mol_string), "  is",  sum_final, "(in amu unit)."
         close(unit=10)
         deallocate(atom_symbl, atom_mass)
        

    contains

     subroutine str2int(input_str,output_int)
        implicit none
        character(len=3) :: input_str, input
        integer :: output_int

        input = trim(input_str)
        read(input,*) output_int
    end subroutine str2int

end program molecular_weight_calculator
