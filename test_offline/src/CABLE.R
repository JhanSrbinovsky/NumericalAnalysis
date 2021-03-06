######################################################################################
### this script writes the namelist (cable.nml) which is required to subsequently  ###
### run CABLE over selected sites. this script is called by run_cable.csh in the   ###
### previous directory and reads the established parameters in main.nml            ###
######################################################################################

source('../main.nml')
library(ncdf) # load netcdf library

cnlist=c() # initialise CABLE namelist
for(site in 1:length(obsfiles)) { # for each site
   newdirname= paste( 'out/', sitenames[site], sep="")
   setwd('../')
   system(paste('mkdir',newdirname)) # create new directory
   # elements to write in cable.nml  
   # following "cnlist[] =", LHS arguments are varibales in the CABLE code
   cnlist[1] = '&cable'
   cnlist[2] = paste('  filename%met = \'', obsfiles[site],'\'',sep='')
   cnlist[3] = '  filename%out = \'out_cable.nc\''
   cnlist[4] = '  filename%log = \'log_cable.txt\''
   cnlist[5] = paste('  filename%restart_in = \'/home/srb001/CABLE-AUX/offline/data/', sitenames[site],'/out_spinup', sitenames[site],'/restart_out.nc\'',sep='')
   #cnlist[5] = paste('  filename%restart_in = \'', '\'', sep='')
   cnlist[6] = '  filename%restart_out = \'./restart_out.nc\''
   cnlist[7] = '  filename%type    = \'/home/srb001/CABLE-AUX/offline/data/surface_data/gridinfo_CSIRO_1x1.nc\''
   cnlist[8] = '  filename%veg    = \'/home/srb001/CABLE-AUX/core/biogeophys/def_veg_params.txt\''
   cnlist[9] = '  filename%soil    = \'/home/srb001/CABLE-AUX/core/biogeophys/def_soil_params.txt\''
   cnlist[10] = '  vegparmnew = .TRUE.  ! using new format when true'
   cnlist[11] = '  soilparmnew = .TRUE.  ! using new format when true'
   cnlist[12] = '  spinup = .FALSE.  ! do we spin up the model?'
   cnlist[13] = '  delsoilM = 0.001   ! allowed variation in soil moisture for spin up'
   cnlist[14] = '  delsoilT = 0.01    ! allowed variation in soil temperature for spin up'
   cnlist[15] = '  output%restart = .false. ! should a restart file be created?'
   cnlist[16] = '  output%met = .TRUE.  ! input met data'
   cnlist[17] = '  output%flux = .TRUE.  ! convective, runoff, NEE'
   cnlist[18] = '  output%soil = .TRUE.  ! soil states'
   cnlist[19] = '  output%snow = .TRUE.  ! snow states'
   cnlist[20] = '  output%radiation = .TRUE.  ! net rad, albedo'
   cnlist[21] = '  output%carbon    = .TRUE.  ! NEE, GPP, NPP, stores' 
   cnlist[22] = '  output%veg       = .TRUE.  ! vegetation states'
   cnlist[23] = '  output%params    = .TRUE.  ! input parameters used to produce run'
   cnlist[24] = '  output%balances  = .TRUE.  ! energy and water balances'
   cnlist[25] = '  check%ranges     = .FALSE.  ! variable ranges, input and output' # not for LittleWashita
   cnlist[26] = '  check%energy_bal = .TRUE.  ! energy balance'
   cnlist[27] = '  check%mass_bal   = .TRUE.  ! water/mass balance'
   cnlist[28] = '  verbose = .TRUE. ! write details of every grid cell init and params to log?'
   cnlist[29] = '  leaps = .TRUE. ! calculate timing with leap years?'
   cnlist[30] = '  logn = 88      ! log file number - declared in input module'
   cnlist[31] = '  fixedCO2 = 350.0   ! if not found in met file, in ppmv'
   cnlist[32] = '  spincasainput = .FALSE.    ! input required to spin casacnp offline'
   cnlist[33] = '  spincasa      = .FALSE.     ! spin casa before running the model if TRUE, and should be set to FALSE if spincasainput = .TRUE.'
   cnlist[34] = '  l_casacnp     = .FALSE.  ! using casaCNP with CABLE '
   cnlist[35] = '  l_laiFeedbk   = .FALSE.  ! using prognostic LAI'   
   cnlist[36] = '  l_vcmaxFeedbk = .FALSE.  ! using prognostic Vcmax'
   cnlist[37] = '  icycle = 0   ! BP pull it out from casadimension and put here; 0 for not using casaCNP, 1 for C, 2 for C+N, 3 for C+N+P'
   cnlist[38] = 'casafile%cnpbiome=\'/home/srb001/CABLE-AUX/core/biogeochem/pftlookup_csiro_v16_17tiles.csv\'  ! biome specific BGC parameters'
   cnlist[39] = 'casafile%cnpepool=\'poolcnpOut.csv\'    ! end of run pool size'
   cnlist[40] = paste('casafile%cnpipool=\'/home/srb001/CABLE-AUX/offline/data/surface_data/poolcnpIn',sitenames[site],'.csv\'    ! initial pool size',sep='')
   cnlist[41] = 'casafile%cnpmetout=\'casamet.nc\'                ! output daily met forcing for spinning casacnp'
   cnlist[42] = 'casafile%cnpmetin=\'/home/srb001/CABLE-AUX/offline/data/surface_data/fcasamet.lst\'          ! list of daily met files for spinning casacnp'
   cnlist[43] = '  casafile%phen=\'/home/srb001/CABLE-AUX/core/biogeochem/modis_phenology_csiro.txt\'        ! modis phenology'
   cnlist[44] = 'casafile%cnpflux=\'cnpfluxOut.csv\''
   cnlist[45] = 'ncciy = 0 ! 0 for not using gswp; 4-digit year input for year of gswp met'
   cnlist[46] = 'gswpfile%rainf = \'gswp/Rainf_gswp1987.nc\''
   cnlist[47] = 'gswpfile%snowf = \'gswp/Snowf_gswp1987.nc\''
   cnlist[48] = 'gswpfile%LWdown= \'gswp/LWdown_srb1987.nc\''
   cnlist[49] = 'gswpfile%SWdown= \'gswp/SWdown_srb1987.nc\''
   cnlist[50] = 'gswpfile%PSurf = \'gswp/PSurf_ecor1987.nc\''
   cnlist[51] = 'gswpfile%Qair  = \'gswp/Qair_cru1987.nc\''
   cnlist[52] = 'gswpfile%Tair  = \'gswp/Tair_cru1987.nc\''
   cnlist[53] = 'gswpfile%wind  = \'gswp/Wind_ncep1987.nc\''
   cnlist[54] = 'redistrb = .FALSE.  ! Turn on/off the hydraulic redistribution'
   cnlist[55] = 'wiltParam = 0.5'
   cnlist[56] = 'satuParam = 0.8'
   cnlist[57] = '  cable_user%FWSOIL_SWITCH = \'standard\'        ! choices are: ' 
   cnlist[58] = '                                                ! 1. standard ' 
   cnlist[59] = '                                                ! 2. non-linear extrapolation '
   cnlist[60] = '                                                ! 3. Lai and Ktaul 2000 '
   cnlist[61] = '  cable_user%DIAG_SOIL_RESP = \'ON \' ' 
   cnlist[62] = '  cable_user%LEAF_RESPIRATION = \'ON \' ' 
   #cnlist[63] = '  cable_user%RUN_DIAG_LEVEL= \'prog\'        ! choices are: ' 
   cnlist[63] = '  cable_user%RUN_DIAG_LEVEL= \'NONE\'        ! choices are: ' 
   cnlist[64] = '                                                ! 1. BASIC' 
   cnlist[65] = '                                                ! 1. NONE' 
   cnlist[66] = '  cable_user%CONSISTENCY_CHECK= .TRUE.      ! TRUE outputs combined fluxes at each timestep for comparisson to A control run ' 
   cnlist[67] = '  cable_user%CASA_DUMP_READ = .FALSE.      ! TRUE reads CASA forcing from netcdf format' 
   cnlist[68] = '  cable_user%CASA_DUMP_WRITE = .FALSE.      ! TRUE outputs CASA forcing in netcdf format' 
   cnlist[69] = '&end'

   write(cnlist,'cable.nml')

   # Run CABLE
   print(paste('Running CABLE at ',sitenames[site],':',sep=''))
   system('./cable') # Will run CABLE

   #system('./cable_ncdf.pl ebal_tot_cncheck00')    
   #system('./cable_ncdf.pl ebal_tot00')
   #system('./cable_ncdf.pl wbal_tot00')
   #system('ncl cable_map_w.ncl')
   #system('ncl cable_map.ncl')

   # move CABLE output to new dir out/"sitename"
   setwd( paste( newdirname,sep='' ) ) # cd to new directory

   # save copies of output to new dir b4 deleting in csh script
      #needed for plotting
      #     out_cable.nc <= CABLE you have just run  
      #     old_cable.nc <= previous version of CABLE to compare against
      #        NB. unless you specify this the csh script will cp the obs. file
      #        to old_cable.nc for the sake of plotCABLE.R
      # by default uses out_cable.nc as old_cable.nc 
      # (can also turn this off in main.nml oldflag=FALSE)
      file.copy('../../out_cable.nc','out_cable.nc')
      file.copy('../../old_cable.nc','old_cable.nc')
   
      # output log file from run
      file.copy('../../log_cable.txt','log_cable.txt')
      # restart file generated from run
      file.copy('../../restart_out.nc','restart_out.nc')
      # make a copy of the namelist file
      file.copy('../../cable.nml','cable.nml')
      # copy the files from CASA-CNP, if generated
      file.copy('../../poolcnpOut.csv','poolcnpOut.csv')
      file.copy('../../cnpfluxOut.csv','cnpfluxOut.csv')
      #file.copy( paste( '../sample_met/', obsfiles[site], sep=''), obsfiles[site] )
      setwd('../')
}





















