# hcm-time-constraint-utils
abap time constraint utilites

This utility takes in a time-dependent internal table and performs CRUD operations 
conserving the configured time constraints rules.

When dealing with HCM infotypes masterdata it is sometimes helpful to detect 
dates collisions when inserting/modifying or deleting infotype records - and even more helpful 
to know in advance the appropiate records' dates manipulation done by the system without
actually changing the records.

E.g : (Dates are given in dd.mm.yyyy format )
  1.  Insertion of 01.01.2017-31.12.9999(1) record to existing state of records:
      01.01.2016 - 31.12.9999(2)
      
      will result in the delimination of record (2) 
      producing this final state: 
      01.01.2016-31.12.2016(2)
      01.01.2017-31.12.9999(1)
      
  2.  Insertion of 01.01.2016-01.01.2017(1) record to existing state of records:
      31.12.2016-31.12.9999(2)
      
      will result in the postponding of record (2)
      producing this final state:
      01.01.2016-01.01.2017(1)
      02.01.2017-31.12.9999(2)
      
  3.  Insertion of 01.01.2015-31.12.9999(1) record to existing state of records:
      01.01.2016-31.12.9999(2)
      
      will result in overriding record (2) entirely
      producing this final state:
      01.01.2015-31.12.9999(1)

