ifort -c test_create_nc.F90 -I/nfs/home/xiezm/easy-netcdf/
ifort -o test_create_nc.exe test_create_nc.o -L/nfs/home/xiezm/easy-netcdf/lib -leasy_netcdf -L//nfs/home/xiezm/easy-netcdf/lib/container -lfortran_container
./test_create_nc.exe
