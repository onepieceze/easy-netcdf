ifort -c test_read_nc.F90 -I/nfs/home/xiezm/easy-netcdf/
ifort -o test_read_nc.exe test_read_nc.o -L/nfs/home/xiezm/easy-netcdf/lib -leasy_netcdf -L//nfs/home/xiezm/easy-netcdf/lib/container -lfortran_container
./test_read_nc.exe
