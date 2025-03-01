program molecular_weight_calculator
       implicit none
       integer :: i, n, p, mol_len, row 
       character(len=20) :: mol_string, j 
       character(len=20), allocatable :: atom_list(:,:)  

       print *, "put your molecular formula (e.g. C2H4)"
       read (*,*) mol_string

       mol_len = len_trim(mol_string)
       
       row = 0
         do i= 1, mol_len
            if (mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') then 
               ! atom_list = mol_string(i:i) 
                  row = row + 1
            end if
         end do
         allocate(atom_list(row,1)) 
 
          n = 0
         do i= 1, mol_len
           if (mol_string(i:i) >= '1' .and. mol_string(i:i) <= '9') then 
             atom_list = mol_string(i:i)   
           end if
         end do
       do i= 1, row  
         print *, atom_list(i,:) 
       end do
       deallocate(atom_list)
end program molecular_weight_calculator
           
