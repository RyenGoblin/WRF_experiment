module purge
module load gcc/7.3.0-wzm
export DIR=/public23/home/sc61341/WTH/allsky_wrfda/LIBRARIES
#Jasper
export JASPERLIB=$DIR/grib2/lib
export JASPERINC=$DIR/grib2/include
export LDFLAGS="-L$DIR/grib2/lib $LDFLAGS"
export CPPFLAGS="-I$DIR/grib2/include $CPPFLAGS"
#netcdfc-4.7.2/fortran-4.5.2
export PATH=$DIR/netcdf/bin:$PATH
export LD_LIBRARY_PATH=$DIR/netcdf/lib:$LD_LIBRARY_PATH
export NETCDF=$DIR/netcdf
export LDFLAGS="-L$NETCDF/lib $LDFLAGS"
export CPPFLAGS="-I$NETCDF/include $CPPFLAGS"
#hdf5-1.10.5
export HDF5=$DIR/hdf5
export LD_LIBRARY_PATH=$DIR/hdf5/lib:$LD_LIBRARY_PATH
export CPPFLAGS="-I$HDF5/include $CPPFLAGS"
export LDFLAGS="-L$HDF5/lib $LDFLAGS"
#mpich-3.0.4
export PATH=$DIR/mpich/bin:$PATH
export LD_LIBRARY_PATH=$DIR/mpich/lib:$LD_LIBRARY_PATH
#RTTOV12.1
export RTTOV=/public23/home/sc61341/WTH/allsky_wrfda/RTTOV_fyde






