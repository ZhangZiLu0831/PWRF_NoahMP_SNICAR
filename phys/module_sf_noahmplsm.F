!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they INclude>
!  Output Files:
!        <list file names and briefly describe the INformation they INclude>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshootINg INstructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

MODULE MODULE_SF_NOAHMPLSM

  use module_sf_gecros, only : gecros
  use shr_kind_mod  , only : r8 => shr_kind_r8 
  use shr_const_mod , only : SHR_CONST_RHOICE,SHR_CONST_PI 
  use module_cam_support, only: endrun !报错

  IMPLICIT NONE

  public  :: noahmp_options
  public  :: NOAHMP_SFLX
  
!  public :: snicar_init
  private :: ATM
  private :: PHENOLOGY
  private :: PRECIP_HEAT
  private :: ENERGY
  private ::       THERMOPROP
  private ::               CSNOW
  private ::               TDFCND
  private ::       RADIATION
  private ::               ALBEDO
  private ::                         SNOW_AGE
  private ::                         SNOWALB_BATS  
  private ::                         SNOWALB_CLASS
  private ::                         SNOWALB_SNICAR_DIR
 ! private ::                         SNOWALB_SNICAR_IDR
  private ::                         GROUNDALB
  private ::                         TWOSTREAM
  private ::               SURRAD
  private ::       VEGE_FLUX
  private ::               SFCDIF1                  
  private ::               SFCDIF2                
  private ::               STOMATA                  
  private ::               CANRES                  
  private ::               ESAT
  private ::               RAGRB
  private ::       BARE_FLUX
  private ::       TSNOSOI
  private ::               HRT
  private ::               HSTEP   
  private ::                         ROSR12
  private ::       PHASECHANGE
  private ::               FRH2O           

  private :: WATER
  private ::       CANWATER
  private ::       SNOWWATER
  private ::               SNOWFALL
  private ::               COMBINE
  private ::               DIVIDE
  private ::                         COMBO
  private ::               COMPACT
  private ::               SNOWH2O
  private ::       SOILWATER
  private ::               ZWTEQ
  private ::               INFIL
  private ::               SRT
  private ::                         WDFCND1        
  private ::                         WDFCND2       
  private ::               SSTEP
  private ::       GROUNDWATER
  private ::       SHALLOWWATERTABLE

  private :: CARBON
  private ::       CO2FLUX
!  private ::       BVOCFLUX
!  private ::       CH4FLUX


  private :: ERROR

! =====================================options for different schemes================================
! **recommended

  INTEGER :: DVEG     ! options for dynamic vegetation: 
                      !   1 -> off (use table LAI; use FVEG = SHDFAC from INput)
                      !   2 -> on  (together with OPT_CRS = 1)
                      !   3 -> off (use table LAI; calculate FVEG)
                      ! **4 -> off (use table LAI; use maximum vegetation fraction)
                      ! **5 -> on  (use maximum vegetation fraction)
                      !   6 -> on  (use FVEG = SHDFAC from INput)
                      !   7 -> off (use INput LAI; use FVEG = SHDFAC from INput)
                      !   8 -> off (use INput LAI; calculate FVEG)
                      !   9 -> off (use INput LAI; use maximum vegetation fraction)
                      !  10 -> crop model on (use maximum vegetation fraction)

  INTEGER :: OPT_CRS  ! options for canopy stomatal resistance
                      ! **1 -> Ball-Berry
		      !   2 -> Jarvis

  INTEGER :: OPT_BTR  ! options for soil moisture factor for stomatal resistance
                      ! **1 -> Noah (soil moisture) 
                      !   2 -> CLM  (matric potential)
                      !   3 -> SSiB (matric potential)

  INTEGER :: OPT_RUN  ! options for runoff and groundwater
                      ! **1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR) ;
                      !   2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR) ;
                      !   3 -> origINal surface and subsurface runoff (free draINage)
                      !   4 -> BATS surface and subsurface runoff (free draINage)
                      !   5 -> Miguez-Macho&Fan groundwater scheme (Miguez-Macho et al. 2007 JGR; Fan et al. 2007 JGR)
		      !          (needs further testINg for public use)

  INTEGER :: OPT_SFC  ! options for surface layer drag coeff (CH & CM)
                      ! **1 -> M-O
		      ! **2 -> origINal Noah (Chen97)
		      ! **3 -> MYJ consistent; 4->YSU consistent. MB: removed IN v3.7 for further testINg

  INTEGER :: OPT_FRZ  ! options for supercooled liquid water (or ice fraction)
                      ! **1 -> no iteration (Niu and Yang, 2006 JHM)
		      !   2 -> Koren's iteration 

  INTEGER :: OPT_INF  ! options for frozen soil permeability
                      ! **1 -> lINear effects, more permeable (Niu and Yang, 2006, JHM)
                      !   2 -> nonlINear effects, less permeable (old)

  INTEGER :: OPT_RAD  ! options for radiation transfer
                      !   1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
                      !   2 -> two-stream applied to grid-cell (gap = 0)
                      ! **3 -> two-stream applied to vegetated fraction (gap=1-FVEG)

  INTEGER :: OPT_ALB  ! options for ground snow surface albedo
                      !   1 -> BATS
		      ! **2 -> CLASS
          ! 3->SNICAR_DIR 4->SNICAR_IDR

  INTEGER :: OPT_SNF  ! options for partitionINg  precipitation INto raINfall & snowfall
                      ! **1 -> Jordan (1991)
		      !   2 -> BATS: when SFCTMP<TFRZ+2.2 
		      !   3 -> SFCTMP < TFRZ
		      !   4 -> Use WRF microphysics OUTput

  INTEGER :: OPT_TBOT ! options for lower boundary condition of soil temperature
                      !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                      ! **2 -> TBOT at ZBOT (8m) read from a file (origINal Noah)

  INTEGER :: OPT_STC  ! options for snow/soil temperature time scheme (only layer 1)
                      ! **1 -> semi-implicit; flux top boundary condition
		      !   2 -> full implicit (origINal Noah); temperature top boundary condition
                      !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)

  INTEGER :: OPT_RSF  ! options for surface resistent to evaporation/sublimation
                      ! **1 -> Sakaguchi and Zeng, 2009
		      !   2 -> Sellers (1992)
                      !   3 -> adjusted Sellers to decrease RSURF for wet soil
		      !   4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set IN MPTABLE); AD v3.8

  INTEGER :: OPT_SOIL ! options for defININg soil properties
                      ! **1 -> use INput domINant soil texture
		      !   2 -> use INput soil texture that varies with depth
                      !   3 -> use soil composition (sand, clay, orgm) and pedotransfer functions (OPT_PEDO)
		      !   4 -> use INput soil properties (BEXP_3D, SMCMAX_3D, etc.)

  INTEGER :: OPT_PEDO ! options for pedotransfer functions (used when OPT_SOIL = 3)
                      ! **1 -> Saxton and Rawls (2006)

  INTEGER :: OPT_CROP ! options for crop model
                      ! **0 -> No crop model, will run default dynamic vegetation
                      !   1 -> Liu, et al. 2016
		      !   2 -> Gecros (Genotype-by-Environment INteraction on CROp growth Simulator) YIN and van Laar, 2005

!------------------------------------------------------------------------------------------!
! Physical Constants:                                                                      !
!------------------------------------------------------------------------------------------!

  REAL, PARAMETER :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  REAL, PARAMETER :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  REAL, PARAMETER :: VKC    = 0.40      !von Karman constant
  REAL, PARAMETER :: TFRZ   = 273.16    !freezINg/meltINg poINt (k)
  REAL, PARAMETER :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  REAL, PARAMETER :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  REAL, PARAMETER :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  REAL, PARAMETER :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  REAL, PARAMETER :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  REAL, PARAMETER :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  REAL, PARAMETER :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  REAL, PARAMETER :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  REAL, PARAMETER :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  REAL, PARAMETER :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  REAL, PARAMETER :: DENH2O = 1000.     !density of water (kg/m3)
  REAL, PARAMETER :: DENICE = 917.      !density of ice (kg/m3)
  
  REAL(r8),PUBLIC, PARAMETER:: snw_rds_min = 54.526_r8  ! minimum allowed snow effective radius (also "fresh snow" value) [microns]
  INTEGER, PRIVATE, PARAMETER :: MBAND = 2
  INTEGER, PRIVATE, PARAMETER :: NSOIL = 4
  INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8
 !INTEGER, PRIVATE, PARAMETER :: numrad_snw  =   5 
  integer,  parameter :: nir_bnd_bgn =   2               ! first band index in near-IR spectrum [idx]
  integer,  parameter :: nir_bnd_end =   5               ! ending near-IR band index [idx]
  integer,  parameter :: snw_rds_max_tbl = 1500          ! maximum effective radius defined in Mie lookup table [microns]
  integer,  parameter :: snw_rds_min_tbl = 30            ! minimium effective radius defined in Mie lookup table [microns]
  real(r8), parameter :: min_snw = 1.0E-30_r8            ! minimum snow mass required for SNICAR RT calculation [kg m-2]
  inTEGER,  parameter :: idx_Mie_snw_mx = 1471           ! number of effectiveradius indices used in Mie lookup table [idx]
  inTEGER,  parameter :: idx_T_max      = 11             ! maxiumum temperature index used in aging lookup table [idx]
  inTEGER,  parameter :: idx_Tgrd_max   = 31             ! maxiumum temperature gradient index used in aging lookup table [idx]
  inTEGER,  parameter :: idx_rhos_max   = 8              ! maxiumum snow density index used in aging lookup table [idx]
  inTEGER,  parameter :: numrad_snw  =   5               ! number of spectral bands used in snow model [nbr]  
  
  ! direct-beam weighted ice optical properties
  REAL(r8) :: ss_alb_snw_drc(1,numrad_snw)
  REAL(r8) :: asm_prm_snw_drc(1,numrad_snw)
  REAL(r8) :: ext_cff_mss_snw_drc(1,numrad_snw)

  ! diffuse radiation weighted ice optical properties
  REAL(r8) :: ss_alb_snw_dfs(1,numrad_snw)
  REAL(r8) :: asm_prm_snw_dfs(1,numrad_snw)
  REAL(r8) :: ext_cff_mss_snw_dfs(1,numrad_snw)

  integer :: ii
  ! hydrophiliic BC
  REAL(r8) :: ss_alb_bc1(1,numrad_snw)
  REAL(r8) :: asm_prm_bc1(1,numrad_snw)
  REAL(r8) :: ext_cff_mss_bc1(1,numrad_snw)
  data(ss_alb_bc1(1,ii),ii=1,5) / 0.515945305512804, 0.434313626536424,&
0.346103765992635, 0.275522926330555, 0.138576096442815/

 data(asm_prm_bc1(1,ii),ii=1,5) / 0.521517715996158, 0.34457189840306,&
0.244048159248401,  0.188518513380877, 0.103316928297739/

 data(ext_cff_mss_bc1(1,ii),ii=1,5) /25368.6111954733, 12520.3846877849,&
7738.643174918, 5744.35461327268, 3526.76546641382/

 data(ss_alb_snw_drc(1,ii),ii=1,5) / 0.9999831498414072, 0.9995291756614075,&
0.9964705192748391, 0.9818258855648294, 0.8220871814520188/

 data(asm_prm_snw_drc(1,ii),ii=1,5) /  0.8897882532850973, 0.8920299520865267,&
0.8934748006796925, 0.9009855353489820, 0.9351691854686424/

 data(ext_cff_mss_snw_drc(1,ii),ii=1,5) /10.9817356228495413, 11.0114519585952344, 11.0332502038050837, 11.0441203664780687, 11.0937756137381918/

 data(ss_alb_snw_dfs(1,ii),ii=1,5) / 0.999983352511756, 0.9995510805977795,& 
0.9965203672025020, 0.9837192364246254, 0.8335358716503068/

 data(asm_prm_snw_dfs(1,ii),ii=1,5) /  0.8897283846697718, 0.8920097381157693,&
0.8934564272918165, 0.8984711196136903, 0.9285450809399972/

 data(ext_cff_mss_snw_dfs(1,ii),ii=1,5) /10.9813297570623636, 11.0111295966681180,11.0329985188683217, 11.0426900405036488, 11.0751974445400805/


  REAL, dimension(idx_Mie_snw_mx*numrad_snw) ::  &
                         xx_ext_cff_mss_snw_dfs  &
                        ,xx_ss_alb_snw_drc       &
                        ,xx_asm_prm_snw_drc      &
                        ,xx_ext_cff_mss_snw_drc  &
                        ,xx_ss_alb_snw_dfs       &
                        ,xx_asm_prm_snw_dfs      

  TYPE noahmp_parameters ! defINe a NoahMP parameters type

!------------------------------------------------------------------------------------------!
! From the veg section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

    LOGICAL :: URBAN_FLAG
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST

    REAL :: CH2OP              !maximum INtercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF              !characteristic leaf dimension (m)
    REAL :: Z0MVT              !momentum roughness length (m)
    REAL :: HVT                !top of canopy (m)
    REAL :: HVB                !bottom of canopy (m)
    REAL :: DEN                !tree density (no. of trunks per m2)
    REAL :: RC                 !tree crown radius (m)
    REAL :: MFSNO              !snowmelt m parameter ()
    REAL :: SAIM(12)           !monthly stem area INdex, one-sided
    REAL :: LAIM(12)           !monthly leaf area INdex, one-sided
    REAL :: SLA                !sINgle-side leaf area per Kg [m2/kg]
    REAL :: DILEFC             !coeficient for leaf stress death [1/s]
    REAL :: DILEFW             !coeficient for leaf stress death [1/s]
    REAL :: FRAGR              !fraction of growth respiration  !origINal was 0.3 
    REAL :: LTOVRC             !leaf turnover [1/s]

    REAL :: C3PSN              !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25               !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC                !q10 for kc25
    REAL :: KO25               !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO                !q10 for ko25
    REAL :: VCMX25             !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX              !q10 for vcmx25
    REAL :: BP                 !mINimum leaf conductance (umol/m**2/s)
    REAL :: MP                 !slope of conductance-to-photosynthesis relationship
    REAL :: QE25               !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE                !q10 for qe25
    REAL :: RMF25              !leaf maINtenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25              !stem maINtenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25              !root maINtenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM                !q10 for maINtenance respiration
    REAL :: FOLNMX             !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN               !mINimum temperature for photosynthesis (k)
       
    REAL :: XL                 !leaf/stem orientation INdex
    REAL :: RHOL(MBAND)        !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS(MBAND)        !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL(MBAND)        !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS(MBAND)        !stem transmittance: 1=vis, 2=nir

    REAL :: MRP                !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT              !empirical canopy wINd parameter

    REAL :: WRRAT              !wood to non-wood ratio
    REAL :: WDPOOL             !wood pool (switch 1 or 0) dependINg on woody or not [-]
    REAL :: TDLEF              !characteristic T for leaf freezINg [K]

  INTEGER :: NROOT              !number of soil layers with root present
     REAL :: RGL                !Parameter used IN radiation stress function
     REAL :: RSMIN              !MINimum stomatal resistance [s m-1]
     REAL :: HS                 !Parameter used IN vapor pressure deficit function
     REAL :: TOPT               !Optimum transpiration air temperature [K]
     REAL :: RSMAX              !Maximal stomatal resistance [s m-1]

     REAL :: SLAREA
     REAL :: EPS(5)

!------------------------------------------------------------------------------------------!
! From the rad section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

     REAL :: ALBSAT(MBAND)       !saturated soil albedos: 1=vis, 2=nir
     REAL :: ALBDRY(MBAND)       !dry soil albedos: 1=vis, 2=nir
     REAL :: ALBICE(MBAND)       !albedo land ice: 1=vis, 2=nir
     REAL :: ALBLAK(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
     REAL :: OMEGAS(MBAND)       !two-stream parameter omega for snow
     REAL :: BETADS              !two-stream parameter betad for snow
     REAL :: BETAIS              !two-stream parameter betad for snow
     REAL :: EG(2)               !emissivity

!------------------------------------------------------------------------------------------!
! From the globals section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
     REAL :: CO2          !co2 partial pressure
     REAL :: O2           !o2 partial pressure
     REAL :: TIMEAN       !gridcell mean topgraphic INdex (global mean)
     REAL :: FSATMX       !maximum surface saturated fraction (global mean)
     REAL :: Z0SNO        !snow surface roughness length (m) (0.002)
     REAL :: SSI          !liquid water holdINg capacity for snowpack (m3/m3)
     REAL :: SWEMX        !new snow mass to fully cover old snow (mm)
     REAL :: TAU0         !tau0 from Yang97 eqn. 10a
     REAL :: GRAIN_GROWTH !growth from vapor diffusion Yang97 eqn. 10b
     REAL :: EXTRA_GROWTH !extra growth near freezINg Yang97 eqn. 10c
     REAL :: DIRT_SOOT    !dirt and soot term Yang97 eqn. 10d
     REAL :: BATS_COSZ    !zenith angle snow albedo adjustment; b IN Yang97 eqn. 15
     REAL :: BATS_VIS_NEW !new snow visible albedo
     REAL :: BATS_NIR_NEW !new snow NIR albedo
     REAL :: BATS_VIS_AGE !age factor for diffuse visible snow albedo Yang97 eqn. 17
     REAL :: BATS_NIR_AGE !age factor for diffuse NIR snow albedo Yang97 eqn. 18
     REAL :: BATS_VIS_DIR !cosz factor for direct visible snow albedo Yang97 eqn. 15
     REAL :: BATS_NIR_DIR !cosz factor for direct NIR snow albedo Yang97 eqn. 16
     REAL :: RSURF_SNOW   !surface resistance for snow(s/m)
     REAL :: RSURF_EXP    !exponent IN the shape parameter for soil resistance option 1

!------------------------------------------------------------------------------------------!
! From the crop section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
  INTEGER :: PLTDAY           ! PlantINg date
  INTEGER :: HSDAY            ! Harvest date
     REAL :: PLANTPOP         ! Plant density [per ha] - used?
     REAL :: IRRI             ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
     REAL :: GDDTBASE         ! Base temperature for GDD accumulation [C]
     REAL :: GDDTCUT          ! Upper temperature for GDD accumulation [C]
     REAL :: GDDS1            ! GDD from seedINg to emergence
     REAL :: GDDS2            ! GDD from seedINg to INitial vegetative 
     REAL :: GDDS3            ! GDD from seedINg to post vegetative 
     REAL :: GDDS4            ! GDD from seedINg to INtial reproductive
     REAL :: GDDS5            ! GDD from seedINg to pysical maturity 
  INTEGER :: C3C4             ! photosynthetic pathway:  1 = c3 2 = c4
     REAL :: AREF             ! reference maximum CO2 assimulation rate 
     REAL :: PSNRF            ! CO2 assimulation reduction factor(0-1) (caused by non-modelINg part,e.g.pest,weeds)
     REAL :: I2PAR            ! Fraction of INcomINg solar radiation to photosynthetically active radiation
     REAL :: TASSIM0          ! MINimum temperature for CO2 assimulation [C]
     REAL :: TASSIM1          ! CO2 assimulation lINearly INcreasINg until temperature reaches T1 [C]
     REAL :: TASSIM2          ! CO2 assmilation rate remaIN at Aref until temperature reaches T2 [C]
     REAL :: K                ! light extINction coefficient
     REAL :: EPSI             ! INitial light use efficiency
     REAL :: Q10MR            ! q10 for maINtaINance respiration
     REAL :: FOLN_MX          ! foliage nitrogen concentration when f(n)=1 (%)
     REAL :: LEFREEZ          ! characteristic T for leaf freezINg [K]
     REAL :: DILE_FC(NSTAGE)  ! coeficient for temperature leaf stress death [1/s]
     REAL :: DILE_FW(NSTAGE)  ! coeficient for water leaf stress death [1/s]
     REAL :: FRA_GR           ! fraction of growth respiration 
     REAL :: LF_OVRC(NSTAGE)  ! fraction of leaf turnover  [1/s]
     REAL :: ST_OVRC(NSTAGE)  ! fraction of stem turnover  [1/s]
     REAL :: RT_OVRC(NSTAGE)  ! fraction of root tunrover  [1/s]
     REAL :: LFMR25           ! leaf maINtenance respiration at 25C [umol CO2/m**2  /s]
     REAL :: STMR25           ! stem maINtenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: RTMR25           ! root maINtenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: GRAINMR25        ! graIN maINtenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: LFPT(NSTAGE)     ! fraction of carbohydrate flux to leaf
     REAL :: STPT(NSTAGE)     ! fraction of carbohydrate flux to stem
     REAL :: RTPT(NSTAGE)     ! fraction of carbohydrate flux to root
     REAL :: GRAINPT(NSTAGE)  ! fraction of carbohydrate flux to graIN
     REAL :: BIO2LAI          ! leaf are per livINg leaf biomass [m^2/kg]

!------------------------------------------------------------------------------------------!
! From the SOILPARM.TBL tables, as functions of soil category.
!------------------------------------------------------------------------------------------!
     REAL :: BEXP(NSOIL)   !B parameter
     REAL :: SMCDRY(NSOIL) !dry soil moisture threshold where direct evap from top
                           !layer ends (volumetric) (not used MB: 20140718)
     REAL :: SMCWLT(NSOIL) !wiltINg poINt soil moisture (volumetric)
     REAL :: SMCREF(NSOIL) !reference soil moisture (field capacity) (volumetric)
     REAL :: SMCMAX(NSOIL) !porosity, saturated value of soil moisture (volumetric)
     REAL :: PSISAT(NSOIL) !saturated soil matric potential
     REAL :: DKSAT(NSOIL)  !saturated soil hydraulic conductivity
     REAL :: DWSAT(NSOIL)  !saturated soil hydraulic diffusivity
     REAL :: QUARTZ(NSOIL) !soil quartz content
     REAL :: F1            !soil thermal diffusivity/conductivity coef (not used MB: 20140718)
!------------------------------------------------------------------------------------------!
! From the GENPARM.TBL file
!------------------------------------------------------------------------------------------!
     REAL :: SLOPE       !slope INdex (0 - 1)
     REAL :: CSOIL       !vol. soil heat capacity [j/m3/K]
     REAL :: ZBOT        !Depth (m) of lower boundary soil temperature
     REAL :: CZIL        !Calculate roughness length of heat
     REAL :: REFDK
     REAL :: REFKDT

     REAL :: KDT         !used IN compute maximum INfiltration rate (IN INFIL)
     REAL :: FRZX        !used IN compute maximum INfiltration rate (IN INFIL)

  END TYPE noahmp_parameters

contains
!BEGIN snicar_init
!!== begIN noahmp_sflx ==============================================================================

  SUBROUTINE NOAHMP_SFLX (parameters, &
                   ILOC    , JLOC    , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
                   SHDFAC  , SHDMAX  , VEGTYP  , ICE     , IST     , CROPTYPE, & ! IN : Vegetation/Soil characteristics
                   SMCEQ   ,  UALB   , BCS     ,                               & ! IN : Vegetation/Soil characteristics
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : ForcINg
                   QC      , SOLDN   , LWDN    ,                               & ! IN : ForcINg
	           PRCPCONV, PRCPNONC, PRCPSHCV, PRCPSNOW, PRCPGRPL, PRCPHAIL, & ! IN : ForcINg
                   TBOT    , CO2AIR  , O2AIR   , FOLN    , FICEOLD , ZLVL    , & ! IN : ForcINg
                   ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   GRAIN   , GDD     , PGS     ,                               & ! IN/OUT 
                   SMCWTD  ,DEEPRECH , RECH    ,                               & ! IN/OUT :
                   GECROS1D,                                                   & ! IN/OUT :
		   Z0WRF   , &
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
		   ALBSND  , ALBSNI  ,                                         & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
		   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
		   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
		   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , &
                   PAHG    , PAHB    , PAH     , LAISUN  , LAISHA  , RB        & ! OUT
#ifdef WRF_HYDRO
                   ,SFCHEADRT                                                  & ! IN/OUT :
#endif
                   )

! --------------------------------------------------------------------------------------------------
! Initial code: Guo-Yue Niu, Oct. 2007
! --------------------------------------------------------------------------------------------------

  implicit none
! --------------------------------------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters

  INTEGER                        , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                        , INTENT(IN)    :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN)    :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN)    :: CROPTYPE !crop type 
  INTEGER                        , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)    :: NSOIL  !no. of soil layers        
  INTEGER                        , INTENT(IN)    :: ILOC   !grid INdex
  INTEGER                        , INTENT(IN)    :: JLOC   !grid INdex
  REAL                           , INTENT(IN)    :: DT     !time step [sec]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)
  REAL                           , INTENT(IN)    :: Q2     !mixINg ratio (kg/kg) lowest model layer
  REAL                           , INTENT(IN)    :: SFCTMP !surface air temperature [K]
  REAL                           , INTENT(IN)    :: UU     !wINd speed IN eastward dir (m/s)
  REAL                           , INTENT(IN)    :: VV     !wINd speed IN northward dir (m/s)
  REAL                           , INTENT(IN)    :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                           , INTENT(INOUT) :: ZLVL   !reference height (m)
  REAL                           , INTENT(IN)    :: COSZ   !cosINe solar zenith angle [0-1]
  REAL                           , INTENT(IN)    :: TBOT   !bottom condition for soil temp. [K]
  REAL                           , INTENT(IN)    :: FOLN   !foliage nitrogen (%) [1-saturated]
  REAL                           , INTENT(IN)    :: SHDFAC !green vegetation fraction [0.0-1.0]
  INTEGER                        , INTENT(IN)    :: YEARLEN!Number of days IN the particular year.
  REAL                           , INTENT(IN)    :: JULIAN !Julian day of year (floatINg poINt)
  REAL                           , INTENT(IN)    :: LAT    !latitude (radians)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ  !equilibrium soil water  content [m3/m3]
  REAL                           , INTENT(IN)    :: PRCPCONV ! convective precipitation enterINg  [mm/s]    ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPNONC ! non-convective precipitation enterINg [mm/s] ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSHCV ! shallow convective precip enterINg  [mm/s]   ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSNOW ! snow enterINg land model [mm/s]              ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPGRPL ! graupel enterINg land model [mm/s]           ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPHAIL ! hail enterINg land model [mm/s]              ! MB/AN : v3.7
  REAL(r8)                       , INTENT(IN)    :: BCS     !bc IN snow (ng/g)
  REAL                           , INTENT(IN)    :: UALB   !Albedo of underlyINg surface
!jref:start; IN 
  REAL                           , INTENT(IN)    :: QC     !cloud water mixINg ratio
  REAL                           , INTENT(INOUT) :: QSFC   !mixINg ratio at lowest model layer
  REAL                           , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(IN)    :: DX
  REAL                           , INTENT(IN)    :: SHDMAX  !yearly max vegetation fraction
!jref:end

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! INput/OUTput : need arbitary INtial values
  REAL                           , INTENT(INOUT) :: QSNOW  !snowfall [mm/s]
  REAL                           , INTENT(INOUT) :: FWET   !wetted or snowed fraction of canopy (-)
  REAL                           , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL                           , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                           , INTENT(INOUT) :: TAH    !canopy air tmeperature (k)
  REAL                           , INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL                           , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                           , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                           , INTENT(INOUT) :: TAUSS  !non-dimensional snow age

! prognostic variables
  INTEGER                        , INTENT(INOUT) :: ISNOW  !actual no. of snow layers [-]
  REAL                           , INTENT(INOUT) :: CANLIQ !INtercepted liquid water (mm)
  REAL                           , INTENT(INOUT) :: CANICE !INtercepted ice mass (mm)
  REAL                           , INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL                           , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL                           , INTENT(INOUT) :: ZWT    !depth to water table [m]
  REAL                           , INTENT(INOUT) :: WA     !water storage IN aquifer [mm]
  REAL                           , INTENT(INOUT) :: WT     !water IN aquifer&saturated soil [mm]
  REAL                           , INTENT(INOUT) :: WSLAKE !lake water storage (can be neg.) (mm)
  REAL,                            INTENT(INOUT) :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
  REAL,                            INTENT(INOUT) :: DEEPRECH !recharge to or from the water table when deep [m]
  REAL,                            INTENT(INOUT) :: RECH !recharge to or from the water table when shallow [m] (diagnostic)

  REAL, DIMENSION(1:60)          , INTENT(INOUT) :: gecros1d !  gecros crop
  
! OUTput
  REAL                           , INTENT(OUT)   :: Z0WRF  !combINed z0 sent to coupled model
  REAL                           , INTENT(OUT)   :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FIRA   !total net LW rad (w/m2)  [+ to atm]
  REAL                           , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCEV   !canopy evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCTR   !transpiration heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL                           , INTENT(OUT)   :: TRAD   !surface radiative temperature (k)
  REAL                                           :: TS     !surface temperature (k)
  REAL                           , INTENT(OUT)   :: ECAN   !evaporation of INtercepted water (mm/s)
  REAL                           , INTENT(OUT)   :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(OUT)   :: EDIR   !soil surface evaporation rate (mm/s]
  REAL                           , INTENT(OUT)   :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(OUT)   :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(OUT)   :: PSN    !total photosynthesis (umol co2/m2/s) [+]
  REAL                           , INTENT(OUT)   :: APAR   !photosyn active energy by canopy (w/m2)
  REAL                           , INTENT(OUT)   :: SAV    !solar rad absorbed by veg. (w/m2)
  REAL                           , INTENT(OUT)   :: SAG    !solar rad absorbed by ground (w/m2)
  REAL                           , INTENT(OUT)   :: FSNO   !snow cover fraction on the ground (-)
  REAL                           , INTENT(OUT)   :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL                           , INTENT(OUT)   :: ALBEDO !surface albedo [-]
  REAL                                           :: ERRWAT !water error [kg m{-2}]
  REAL                           , INTENT(OUT)   :: QSNBOT !snowmelt OUT bottom of pack [mm/s]
  REAL                           , INTENT(OUT)   :: PONDING!surface pondINg [mm]
  REAL                           , INTENT(OUT)   :: PONDING1!surface pondINg [mm]
  REAL                           , INTENT(OUT)   :: PONDING2!surface pondINg [mm]
  REAL                           , INTENT(OUT)   :: RB        ! leaf boundary layer resistance (s/m)
  REAL                           , INTENT(OUT)   :: LAISUN    ! sunlit leaf area INdex (m2/m2)
  REAL                           , INTENT(OUT)   :: LAISHA    ! shaded leaf area INdex (m2/m2)

!jref:start; OUTput
  REAL                           , INTENT(OUT)     :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                           , INTENT(OUT)     :: T2MB   !2-m air temperature over bare ground part [k]
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSNI   !snow albedo (diffuse)
  REAL, INTENT(OUT) :: TGV
  REAL, INTENT(OUT) :: TGB
  REAL              :: Q1
  REAL, INTENT(OUT) :: EMISSI
!jref:end

! local
  INTEGER                                        :: IZ     !do-loop INdex
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  !phase change INdex [1-melt; 2-freeze]
  REAL                                           :: CMC    !INtercepted water (CANICE+CANLIQ) (mm)
  REAL                                           :: TAUX   !wINd stress: e-w (n/m2)
  REAL                                           :: TAUY   !wINd stress: n-s (n/m2)
  REAL                                           :: RHOAIR !density air (kg/m3)
!  REAL, DIMENSION(       1:    5)                :: VOCFLX !voc fluxes [ug C m-2 h-1]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO !snow/soil layer thickness [m]
  REAL                                           :: THAIR  !potential temperature (k)
  REAL                                           :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                                           :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:    2)                :: SOLAD  !INcomINg direct solar rad (w/m2)
  REAL, DIMENSION(       1:    2)                :: SOLAI  !INcomINg diffuse solar rad (w/m2)
  REAL                                           :: QPRECC !convective precipitation (mm/s)
  REAL                                           :: QPRECL !large-scale precipitation (mm/s)
  REAL                                           :: IGS    !growINg season INdex (0=off, 1=on)
  REAL                                           :: ELAI   !leaf area INdex, after buryINg by snow
  REAL                                           :: ESAI   !stem area INdex, after buryINg by snow
  REAL                                           :: BEVAP  !soil water evaporation factor (0 - 1)
  REAL, DIMENSION(       1:NSOIL)                :: BTRANI !Soil water transpiration factor (0 - 1)
  REAL                                           :: BTRAN  !soil water transpiration factor (0 - 1)
  REAL                                           :: QIN    !groundwater recharge [mm/s]
  REAL                                           :: QDIS   !groundwater discharge [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: SICE   !soil ice content (m3/m3)
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV !partial volume ice of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV !partial volume liq of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  !effective porosity [m3/m3]
  REAL                                           :: TOTSC  !total soil carbon (g/m2)
  REAL                                           :: TOTLB  !total livINg carbon (g/m2)
  REAL                                           :: T2M    !2-meter air temperature (k)
  REAL                                           :: QDEW   !ground surface dew rate [mm/s]
  REAL                                           :: QVAP   !ground surface evap. rate [mm/s]
  REAL                                           :: LATHEA !latent heat [j/kg]
  REAL                                           :: SWDOWN !downward solar [w/m2]
  REAL                                           :: QMELT  !snowmelt [mm/s]
  REAL                                           :: BEG_WB !water storage at begIN of a step [mm]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL, INTENT(OUT)   :: FPICE   !snow fraction IN precipitation
  REAL, INTENT(OUT)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(OUT)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(OUT)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL, INTENT(OUT)                                           :: PAH     !precipitation advected heat - total (W/m2)

!jref:start 
  REAL                                           :: FSRV
  REAL                                           :: FSRG
  REAL,INTENT(OUT)                               :: Q2V
  REAL,INTENT(OUT)                               :: Q2B
  REAL :: Q2E
  REAL :: QFX
  REAL,INTENT(OUT)                               :: CHV    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB    !sensible heat exchange coefficient over bare-ground
  REAL,INTENT(OUT)                               :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                               :: CHUC   !under canopy exchange coefficient
  REAL,INTENT(OUT)                               :: CHV2    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB2    !sensible heat exchange coefficient over bare-ground
!jref:end  

! carbon
! INputs
  REAL                           , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                           , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)
!  REAL                           , INTENT(IN)    :: BCS     !bc IN snow (ng/g)
!  REAL                           , INTENT(IN)    :: UALB   !Albedo of underlyINg surface
! INputs and OUTputs : prognostic variables
  REAL                        , INTENT(INOUT)    :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT)    :: RTMASS !mass of fINe roots [g/m2]
  REAL                        , INTENT(INOUT)    :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT)    :: WOOD   !mass of wood (INcl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT)    :: STBLCP !stable carbon IN deep soil [g/m2]
  REAL                        , INTENT(INOUT)    :: FASTCP !short-lived carbon, shallow soil [g/m2]
  REAL                        , INTENT(INOUT)    :: LAI    !leaf area INdex [-]
  REAL                        , INTENT(INOUT)    :: SAI    !stem area INdex [-]
  REAL                        , INTENT(INOUT)    :: GRAIN  !graIN mass [g/m2]
  REAL                        , INTENT(INOUT)    :: GDD    !growINg degree days
  INTEGER                     , INTENT(INOUT)    :: PGS    !plant growINg stage [-]

! OUTputs
  REAL                          , INTENT(OUT)    :: NEE    !net ecosys exchange (g/m2/s CO2)
  REAL                          , INTENT(OUT)    :: GPP    !net INstantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT)    :: NPP    !net primary productivity [g/m2/s C]
  REAL                                           :: AUTORS !net ecosystem respiration (g/m2/s C)
  REAL                                           :: HETERS !organic respiration (g/m2/s C)
  REAL                                           :: TROOT  !root-zone averaged temperature (k)
  REAL                                           :: BDFALL   !bulk density of new snow (kg/m3)    ! MB/AN: v3.7
  REAL                                           :: RAIN     !raIN rate                   (mm/s)  ! MB/AN: v3.7
  REAL                                           :: SNOW     !liquid equivalent snow rate (mm/s)  ! MB/AN: v3.7
  REAL                                           :: FP                                            ! MB/AN: v3.7
  REAL                                           :: PRCP                                          ! MB/AN: v3.7
!more local variables for precip heat MB
  REAL                                           :: QINTR   !INterception rate for raIN (mm/s)
  REAL                                           :: QDRIPR  !drip rate for raIN (mm/s)
  REAL                                           :: QTHROR  !throughfall for raIN (mm/s)
  REAL                                           :: QINTS   !INterception (loadINg) rate for snowfall (mm/s)
  REAL                                           :: QDRIPS  !drip (unloadINg) rate for INtercepted snow (mm/s)
  REAL                                           :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                                           :: QRAIN   !raIN at ground srf (mm/s) [+]
  REAL                                           :: SNOWHIN !snow depth INcreasINg rate (m/s)
  REAL                                 :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                                 :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                             :: FROZEN_GROUND ! used to defINe latent heat pathway
  LOGICAL                             :: FROZEN_CANOPY ! used to defINe latent heat pathway
  LOGICAL                             :: dveg_active ! flag to run dynamic vegetation
  LOGICAL                             :: crop_active ! flag to run crop model
  
  REAL :: FB
  
  ! INTENT (OUT) variables need to be assigned a value.  These normally get assigned values
  ! only if DVEG == 2.
  nee = 0.0
  npp = 0.0
  gpp = 0.0
      PAHV  = 0.
      PAHG  = 0.
      PAHB  = 0.
      PAH  = 0.

! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcINg

   CALL ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                            &
             PRCPCONV, PRCPNONC,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL, &
             SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                   & 
             EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   ,SOLAI   , &
             SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      ,FPICE   , PRCP )     

! snow/soil layer thickness (m)

     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO

! root-zone temperature

     TROOT  = 0.
     DO IZ=1,parameters%NROOT
        TROOT = TROOT + STC(IZ)*DZSNSO(IZ)/(-ZSOIL(parameters%NROOT))
     ENDDO

! total water storage for water balance check
    
     IF(IST == 1) THEN
     BEG_WB = CANLIQ + CANICE + SNEQV + WA
     DO IZ = 1,NSOIL
        BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
     END DO
     END IF

! vegetation phenology

     CALL PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !IN
                     LAI    , SAI    , TROOT  , ELAI    , ESAI   ,IGS, PGS)

!INput GVF should be consistent with LAI
     IF(DVEG == 1 .or. DVEG == 6 .or. DVEG == 7) THEN
        FVEG = SHDFAC
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 2 .or. DVEG == 3 .or. DVEG == 8) THEN
        FVEG = 1.-EXP(-0.52*(LAI+SAI))
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 4 .or. DVEG == 5 .or. DVEG == 9) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE
        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
        CALL wrf_error_fatal("Namelist parameter DVEG unknown") 
     ENDIF
     IF(OPT_CROP > 0 .and. CROPTYPE > 0) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.05) FVEG = 0.05
     ENDIF
     IF(parameters%urban_flag .OR. VEGTYP == parameters%ISBARREN) FVEG = 0.0
     IF(ELAI+ESAI == 0.0) FVEG = 0.0

    CALL PRECIP_HEAT(parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !IN
                     ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !IN
                     BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !IN
                     CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !IN
                     QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !OUT
                     PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !OUT
	             FWET   ,CMC                                    )   !OUT

! compute energy budget (momentum & energy fluxes and phase changes) 

    CALL ENERGY (parameters,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !IN
                 ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !IN
                 SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZLVL   , & !IN
                 CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !IN
                 EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  , & !IN
                 ELAI   ,ESAI   ,FWET   ,FOLN   , UALB, BCS,        & !IN
                 FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !IN
                 QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,iloc, jloc , & !IN
		            Z0WRF  ,                                         &
                 IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !OUT
                 SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !OUT
                 TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !OUT
                 TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !OUT
                 PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                         & !OUT
                 TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !INOUT
                 SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !INOUT
                 ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , & !INOUT
                 TAUSS  ,LAISUN ,LAISHA ,RB ,                     & !INOUT
!jref:start
                 QC     ,QSFC   ,PSFC   , & !IN 
                 T2MV   ,T2MB  ,FSRV   , &
                 FSRG   ,RSSUN   ,RSSHA ,ALBSND  ,ALBSNI, BGAP   ,WGAP,TGV,TGB,&
                 Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV   ,CHB     , & !OUT
                 EMISSI ,PAH    ,                                 &
		 SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2,&
                 JULIAN, SWDOWN, PRCP, FB, GECROS1D )        
!jref:end

    SICE(:) = MAX(0.0, SMC(:) - SH2O(:))   
    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEAG, 0.)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW = ABS( MIN(FGEV/LATHEAG, 0.))  ! negative part of fgev
    EDIR = QVAP - QDEW

! compute water budgets (water storages, ET components, and runoff)


     CALL WATER (parameters,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,iloc,jloc , SMCEQ , & !in
                 BDFALL ,FP     ,RAIN   ,SNOW   ,                 & !in  MB/AN: v3.7
		 QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,  & !in  MB
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                 SMCWTD ,DEEPRECH,RECH                          , & !inout
                 CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                 QIN    ,QDIS   ,PONDING1       ,PONDING2,&
                 QSNBOT                             &
#ifdef WRF_HYDRO
                        ,sfcheadrt                     &
#endif
                 )  !out

!     write(*,'(a20,10F15.5)') 'SFLX:RUNOFF=',RUNSRF*DT,RUNSUB*DT,EDIR*DT

! compute carbon budgets (carbon storages and co2 & bvoc fluxes)

   crop_active = .false.
   dveg_active = .false.
   IF (DVEG == 2 .OR. DVEG == 5 .OR. DVEG == 6) dveg_active = .true.
   IF (OPT_CROP > 0 .and. CROPTYPE > 0) THEN
     crop_active = .true.
     dveg_active = .false.
   ENDIF

   IF (dveg_active) THEN
     CALL CARBON (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !in
                 DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                 FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                 TROOT  ,IST    ,LAT    ,iloc   ,jloc   , & !in
                 LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !inout
                 GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                 TOTLB  ,LAI    ,SAI    )                   !out
   END IF

   IF (OPT_CROP == 1 .and. crop_active) THEN
    CALL CARBON_CROP (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  ,JULIAN , & !in 
                         DZSNSO ,STC    ,SMC    ,TV     ,PSN    ,FOLN   ,BTRAN  , & !in
			 SOLDN  ,T2M    ,                                         & !in
                         LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP ,GRAIN  , & !inout
			 LAI    ,SAI    ,GDD    ,                                 & !inout
                         GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  ,TOTLB, PGS    ) !out
   END IF
   
! water and energy balance check

     CALL ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , & !in
                 FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , & !in
                 SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , & !in
                 ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , & !in
                 NSNOW  ,IST    ,ERRWAT ,ILOC   , JLOC  ,FVEG   , &
                 SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                 PAHV   ,PAHG   ,PAHB   )   !in ( Except ERRWAT, which is out )

! urban - jref
    QFX = ETRAN + ECAN + EDIR
    IF ( parameters%urban_flag ) THEN
       QSFC = QFX/(RHOAIR*CH) + QAIR
       Q2B = QSFC
    END IF
    
    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_SFLX

!== begIN atm ======================================================================================

  SUBROUTINE ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                             &
                  PRCPCONV,PRCPNONC ,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL , &
                  SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                    & 
                  EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   , SOLAI   , &
		  SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      , FPICE   ,PRCP )     
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcINg
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs

  type (noahmp_parameters), INTENT(IN) :: parameters
  REAL                          , INTENT(IN)  :: SFCPRS !pressure (pa)
  REAL                          , INTENT(IN)  :: SFCTMP !surface air temperature [k]
  REAL                          , INTENT(IN)  :: Q2     !mixINg ratio (kg/kg)
  REAL                          , INTENT(IN)  :: PRCPCONV ! convective precipitation enterINg  [mm/s]    ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPNONC ! non-convective precipitation enterINg [mm/s] ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSHCV ! shallow convective precip enterINg  [mm/s]   ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSNOW ! snow enterINg land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPGRPL ! graupel enterINg land model [mm/s]           ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPHAIL ! hail enterINg land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                          , INTENT(IN)  :: COSZ   !cosINe solar zenith angle [0-1]

! OUTputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: QPRECC !convective precipitation (mm/s)
  REAL                          , INTENT(OUT) :: QPRECL !large-scale precipitation (mm/s)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !INcomINg direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !INcomINg diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                          , INTENT(OUT) :: BDFALL  !!bulk density of snowfall (kg/m3) AJN
  REAL                          , INTENT(OUT) :: RAIN    !raINfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: SNOW    !liquid equivalent snowfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: FP      !fraction of area receivINg precipitation  AJN
  REAL                          , INTENT(OUT) :: FPICE   !fraction of ice                AJN
  REAL                          , INTENT(OUT) :: PRCP    !total precipitation [mm/s]     ! MB/AN : v3.7

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
  REAL                                        :: PRCP_FROZEN   !total frozen precipitation [mm/s] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7
! --------------------------------------------------------------------------------------------------

!jref: seems like PAIR should be P1000mb??
       PAIR   = SFCPRS                   ! atm bottom level pressure (pa)
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

       QAIR   = Q2                       ! In WRF, driver converts to specific humidity

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
       SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
       SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
       SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir

       PRCP = PRCPCONV + PRCPNONC + PRCPSHCV

       IF(OPT_SNF == 4) THEN
         QPRECC = PRCPCONV + PRCPSHCV
	 QPRECL = PRCPNONC
       ELSE
         QPRECC = 0.10 * PRCP          ! should be from the atmospheric model
         QPRECL = 0.90 * PRCP          ! should be from the atmospheric model
       END IF

! fractional area that receives precipitation (see, Niu et al. 2005)
   
    FP = 0.0
    IF(QPRECC + QPRECL > 0.) & 
       FP = (QPRECC + QPRECL) / (10.*QPRECC + QPRECL)

! partition precipitation INto raIN and snow. Moved from CANWAT MB/AN: v3.7

! Jordan (1991)

     IF(OPT_SNF == 1) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59))       !MB/AN: change to MIN  
     IF(OPT_SNF == 4) THEN
        PRCP_FROZEN = PRCPSNOW + PRCPGRPL + PRCPHAIL
        IF(PRCPNONC > 0. .and. PRCP_FROZEN > 0.) THEN
	  FPICE = MIN(1.0,PRCP_FROZEN/PRCPNONC)
	  FPICE = MAX(0.0,FPICE)
	  BDFALL = BDFALL*(PRCPSNOW/PRCP_FROZEN) + RHO_GRPL*(PRCPGRPL/PRCP_FROZEN) + &
	             RHO_HAIL*(PRCPHAIL/PRCP_FROZEN)
	ELSE
	  FPICE = 0.0
        ENDIF
	
     ENDIF

     RAIN   = PRCP * (1.-FPICE)
     SNOW   = PRCP * FPICE


  END SUBROUTINE ATM

!== begIN phenology ================================================================================

  SUBROUTINE PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !IN
                        LAI    , SAI    , TROOT  , ELAI    , ESAI   , IGS, PGS)

! --------------------------------------------------------------------------------------------------
! vegetation phenology considerINg vegeation canopy beINg buries by snow and evolution IN time
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                , INTENT(IN   ) :: VEGTYP !vegetation type 
  INTEGER                , INTENT(IN   ) :: CROPTYPE !vegetation type 
  REAL                   , INTENT(IN   ) :: SNOWH  !snow height [m]
  REAL                   , INTENT(IN   ) :: TV     !vegetation temperature (k)
  REAL                   , INTENT(IN   ) :: LAT    !latitude (radians)
  INTEGER                , INTENT(IN   ) :: YEARLEN!Number of days IN the particular year
  REAL                   , INTENT(IN   ) :: JULIAN !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
  REAL                   , INTENT(IN   ) :: TROOT  !root-zone averaged temperature (k)
  REAL                   , INTENT(INOUT) :: LAI    !LAI, unadjusted for buryINg by snow
  REAL                   , INTENT(INOUT) :: SAI    !SAI, unadjusted for buryINg by snow

! OUTputs
  REAL                   , INTENT(OUT  ) :: ELAI   !leaf area INdex, after buryINg by snow
  REAL                   , INTENT(OUT  ) :: ESAI   !stem area INdex, after buryINg by snow
  REAL                   , INTENT(OUT  ) :: IGS    !growINg season INdex (0=off, 1=on)
  INTEGER                , INTENT(IN   ) :: PGS    !plant growINg stage

! locals

  REAL                                   :: DB     !thickness of canopy buried by snow (m)
  REAL                                   :: FB     !fraction of canopy buried by snow
  REAL                                   :: SNOWHC !critical snow depth at which short vege
                                                   !is fully covered by snow

  INTEGER                                :: K       !INdex
  INTEGER                                :: IT1,IT2 !INterpolation months
  REAL                                   :: DAY     !current day of year ( 0 <= DAY < YEARLEN )
  REAL                                   :: WT1,WT2 !INterpolation weights
  REAL                                   :: T       !current month (1.00, ..., 12.00)
! --------------------------------------------------------------------------------------------------

IF (CROPTYPE == 0) THEN

  IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

     IF (LAT >= 0.) THEN
        ! Northern Hemisphere
        DAY = JULIAN
     ELSE
        ! SOUThern Hemisphere.  DAY is shifted by 1/2 year.
        DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
     ENDIF

     T = 12. * DAY / REAL(YEARLEN)
     IT1 = T + 0.5
     IT2 = IT1 + 1
     WT1 = (IT1+0.5) - T
     WT2 = 1.-WT1
     IF (IT1 .LT.  1) IT1 = 12
     IF (IT2 .GT. 12) IT2 = 1

     LAI = WT1*parameters%LAIM(IT1) + WT2*parameters%LAIM(IT2)
     SAI = WT1*parameters%SAIM(IT1) + WT2*parameters%SAIM(IT2)
  ENDIF

  IF(DVEG == 7 .or. DVEG == 8 .or. DVEG == 9) THEN
    SAI = MAX(0.05,0.1 * LAI)  ! when readINg LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
    IF (LAI < 0.05) SAI = 0.0  ! if LAI below mINimum, make sure SAI = 0
  ENDIF

  IF (SAI < 0.05) SAI = 0.0                    ! MB: SAI CHECK, change to 0.05 v3.6
  IF (LAI < 0.05 .OR. SAI == 0.0) LAI = 0.0  ! MB: LAI CHECK

  IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
       ( VEGTYP == parameters%ISICE   ) .or. ( parameters%urban_flag ) ) THEN
     LAI  = 0.
     SAI  = 0.
  ENDIF

ENDIF   ! CROPTYPE == 0

!buried by snow

     DB = MIN( MAX(SNOWH - parameters%HVB,0.), parameters%HVT-parameters%HVB )
     FB = DB / MAX(1.E-06,parameters%HVT-parameters%HVB)

     IF(parameters%HVT> 0. .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
       SNOWHC = parameters%HVT*EXP(-SNOWH/0.2)             !      changes to HVT IN MPTABLE
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.-FB)
     ESAI =  SAI*(1.-FB)
     IF (ESAI < 0.05 .and. CROPTYPE == 0) ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
     IF ((ELAI < 0.05 .OR. ESAI == 0.0) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK

! set growINg season flag

     IF ((TV .GT. parameters%TMIN .and. CROPTYPE == 0).or.(PGS > 2 .and. PGS < 7 .and. CROPTYPE > 0)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

  END SUBROUTINE PHENOLOGY

!== begIN precip_heat ==============================================================================

  SUBROUTINE PRECIP_HEAT (parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !IN
                          ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !IN
                          BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !IN
                          CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !IN
                          QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !OUT
			  PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !OUT
			  FWET   ,CMC                                    )   !OUT

! ------------------------ code history ------------------------------
! Michael Barlage: Oct 2013 - split CANWATER to calculate precip movement for 
!                             trackINg of advected heat
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ INput/OUTput variables --------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid INdex
  INTEGER,INTENT(IN)  :: JLOC    !grid INdex
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  INTEGER,INTENT(IN)  :: IST     !surface type 1-soil; 2-lake
  REAL,   INTENT(IN)  :: DT      !maIN time step (s)
  REAL,   INTENT(IN)  :: UU      !u-direction wINd speed [m/s]
  REAL,   INTENT(IN)  :: VV      !v-direction wINd speed [m/s]
  REAL,   INTENT(IN)  :: ELAI    !leaf area INdex, after buryINg by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area INdex, after buryINg by snow
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  REAL,   INTENT(IN)  :: BDFALL  !bulk density of snowfall (kg/m3)
  REAL,   INTENT(IN)  :: RAIN    !raINfall (mm/s)
  REAL,   INTENT(IN)  :: SNOW    !snowfall (mm/s)
  REAL,   INTENT(IN)  :: FP      !fraction of the gridcell that receives precipitation
  REAL,   INTENT(IN)  :: TV      !vegetation temperature (k)
  REAL,   INTENT(IN)  :: SFCTMP  !model-level temperature (k)
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)

! INput & OUTput
  REAL, INTENT(INOUT) :: CANLIQ  !INtercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !INtercepted ice mass (mm)

! OUTput
  REAL, INTENT(OUT)   :: QINTR   !INterception rate for raIN (mm/s)
  REAL, INTENT(OUT)   :: QDRIPR  !drip rate for raIN (mm/s)
  REAL, INTENT(OUT)   :: QTHROR  !throughfall for raIN (mm/s)
  REAL, INTENT(OUT)   :: QINTS   !INterception (loadINg) rate for snowfall (mm/s)
  REAL, INTENT(OUT)   :: QDRIPS  !drip (unloadINg) rate for INtercepted snow (mm/s)
  REAL, INTENT(OUT)   :: QTHROS  !throughfall of snowfall (mm/s)
  REAL, INTENT(OUT)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(OUT)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(OUT)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL, INTENT(OUT)   :: QRAIN   !raIN at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: SNOWHIN !snow depth INcreasINg rate (m/s)
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: CMC     !INtercepted water (mm)
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow INterception (mm)
  REAL                :: MAXLIQ  !canopy capacity for raIN INterception (mm)
  REAL                :: FT      !temperature factor for unloadINg rate
  REAL                :: FV      !wINd factor for unloadINg rate
  REAL                :: PAH_AC  !precipitation advected heat - air to canopy (W/m2)
  REAL                :: PAH_CG  !precipitation advected heat - canopy to ground (W/m2)
  REAL                :: PAH_AG  !precipitation advected heat - air to ground (W/m2)
  REAL                :: ICEDRIP !canice unloadINg
! --------------------------------------------------------------------
! INitialization

      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.
      QTHROS  = 0.
      PAH_AC  = 0.
      PAH_CG  = 0.
      PAH_AG  = 0.
      PAHV    = 0.
      PAHG    = 0.
      PAHB    = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ICEDRIP = 0.0
!      prINt*, "precip_heat begIN canopy balance:",canliq+canice+(raIN+snow)*dt
!      prINt*,  "precip_heat snow*3600.0:",snow*3600.0
!      prINt*,  "precip_heat raIN*3600.0:",raIN*3600.0
!      prINt*,  "precip_heat canice:",canice
!      prINt*,  "precip_heat canliq:",canliq

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! average INterception and throughfall

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  ! INterception capability
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
         CANLIQ=MAX(0.,CANLIQ+QINTR*DT)
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
	 IF(CANLIQ > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPR = QDRIPR + CANLIQ/DT
	   CANLIQ = 0.0
	 END IF
      END IF
      
! heat transported by liquid water

      PAH_AC = FVEG * RAIN * (CWAT/1000.0) * (SFCTMP - TV)
      PAH_CG = QDRIPR * (CWAT/1000.0) * (TV - TG)
      PAH_AG = QTHROR * (CWAT/1000.0) * (SFCTMP - TG)
!      prINt*, "precip_heat PAH_AC:",PAH_AC
!      prINt*, "precip_heat PAH_CG:",PAH_CG
!      prINt*, "precip_heat PAH_AG:",PAH_AG

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
	 ! MB: changed below to reflect the raIN assumption that all precip gets INtercepted 
	 ICEDRIP = MAX(0.,CANICE) * (FV+FT)    !MB: removed /DT
         QDRIPS = (FVEG * SNOW - QINTS) + ICEDRIP
         QTHROS = (1.0-FVEG) * SNOW
         CANICE= MAX(0.,CANICE + (QINTS - ICEDRIP)*DT)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
	 IF(CANICE > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPS = QDRIPS + CANICE/DT
	   CANICE = 0.0
	 END IF
      ENDIF
!      prINt*, "precip_heat canopy through:",3600.0*(FVEG * SNOW - QINTS)
!      prINt*, "precip_heat canopy drip:",3600.0*MAX(0.,CANICE) * (FV+FT)

! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! total canopy water

      CMC = CANLIQ + CANICE

! heat transported by snow/ice

      PAH_AC = PAH_AC +  FVEG * SNOW * (CICE/1000.0) * (SFCTMP - TV)
      PAH_CG = PAH_CG + QDRIPS * (CICE/1000.0) * (TV - TG)
      PAH_AG = PAH_AG + QTHROS * (CICE/1000.0) * (SFCTMP - TG)
      
      PAHV = PAH_AC - PAH_CG
      PAHG = PAH_CG
      PAHB = PAH_AG
      
      IF (FVEG > 0.0 .AND. FVEG < 1.0) THEN
        PAHG = PAHG / FVEG         ! these will be multiplied by fraction later
	PAHB = PAHB / (1.0-FVEG)
      ELSEIF (FVEG <= 0.0) THEN
        PAHB = PAHG + PAHB         ! for case of canopy gettINg buried
        PAHG = 0.0
	PAHV = 0.0
      ELSEIF (FVEG >= 1.0) THEN
	PAHB = 0.0
      END IF
      
      PAHV = MAX(PAHV,-20.0)       ! Put some artificial limits here for stability
      PAHV = MIN(PAHV,20.0)
      PAHG = MAX(PAHG,-20.0)
      PAHG = MIN(PAHG,20.0)
      PAHB = MAX(PAHB,-20.0)
      PAHB = MIN(PAHB,20.0)
      
!      prINt*, 'precip_heat sfctmp,tv,tg:',sfctmp,tv,tg
!      prINt*, 'precip_heat 3600.0*qINts+qdrips+qthros:',3600.0*(qINts+qdrips+qthros)
!      prINt*, "precip_heat maxsno:",maxsno
!      prINt*, "precip_heat PAH_AC:",PAH_AC
!      prINt*, "precip_heat PAH_CG:",PAH_CG
!      prINt*, "precip_heat PAH_AG:",PAH_AG
      
!      prINt*, "precip_heat PAHV:",PAHV
!      prINt*, "precip_heat PAHG:",PAHG
!      prINt*, "precip_heat PAHB:",PAHB
!      prINt*, "precip_heat fveg:",fveg
!      prINt*,  "precip_heat qINts*3600.0:",qINts*3600.0
!      prINt*,  "precip_heat qdrips*3600.0:",qdrips*3600.0
!      prINt*,  "precip_heat qthros*3600.0:",qthros*3600.0
      
! raIN or snow on the ground

      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL

      IF (IST == 2 .AND. TG > TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF
!      prINt*,  "precip_heat qsnow*3600.0:",qsnow*3600.0
!      prINt*,  "precip_heat qraIN*3600.0:",qraIN*3600.0
!      prINt*,  "precip_heat SNOWHIN:",SNOWHIN
!      prINt*,  "precip_heat canice:",canice
!      prINt*,  "precip_heat canliq:",canliq
!      prINt*, "precip_heat end canopy balance:",canliq+canice+(qraIN+qsnow)*dt
      

  END SUBROUTINE PRECIP_HEAT

!== begIN error ====================================================================================

  SUBROUTINE ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , &
                    FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , &
                    SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , &
                    ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , &
                    NSNOW  ,IST    ,ERRWAT, ILOC   ,JLOC   ,FVEG   , &
                    SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                    PAHV   ,PAHG   ,PAHB   )
! --------------------------------------------------------------------------------------------------
! check surface energy balance and water balance
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN) :: ILOC   !grid INdex
  INTEGER                        , INTENT(IN) :: JLOC   !grid INdex
  REAL                           , INTENT(IN) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                           , INTENT(IN) :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FIRA   !total net longwave rad (w/m2)  [+ to atm]
  REAL                           , INTENT(IN) :: FSH    !total sensible heat (w/m2)     [+ to atm]
  REAL                           , INTENT(IN) :: FCEV   !canopy evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FGEV   !ground evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FCTR   !transpiration heat flux (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: SSOIL  !ground heat flux (w/m2)        [+ to soil]
  REAL                           , INTENT(IN) :: FVEG
  REAL                           , INTENT(IN) :: SAV
  REAL                           , INTENT(IN) :: SAG
  REAL                           , INTENT(IN) :: FSRV
  REAL                           , INTENT(IN) :: FSRG
  REAL                           , INTENT(IN) :: ZWT

  REAL                           , INTENT(IN) :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                           , INTENT(IN) :: ECAN   !evaporation of INtercepted water (mm/s)
  REAL                           , INTENT(IN) :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(IN) :: EDIR   !soil surface evaporation rate[mm/s]
  REAL                           , INTENT(IN) :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(IN) :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(IN) :: CANLIQ !INtercepted liquid water (mm)
  REAL                           , INTENT(IN) :: CANICE !INtercepted ice mass (mm)
  REAL                           , INTENT(IN) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL                           , INTENT(IN) :: WA     !water storage IN aquifer [mm]
  REAL                           , INTENT(IN) :: DT     !time step [sec]
  REAL                           , INTENT(IN) :: BEG_WB !water storage at begIN of a timesetp [mm]
  REAL                           , INTENT(OUT) :: ERRWAT !error IN water balance [mm/timestep]
  REAL, INTENT(IN)   :: PAH     !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - total (W/m2)

  INTEGER                                     :: IZ     !do-loop INdex
  REAL                                        :: END_WB !water storage at end of a timestep [mm]
  !KWM REAL                                        :: ERRWAT !error IN water balance [mm/timestep]
  REAL                                        :: ERRENG !error IN surface energy balance [w/m2]
  REAL                                        :: ERRSW  !error IN shortwave radiation balance [w/m2]
  REAL                                        :: FSRVG
  CHARACTER(len=256)                          :: message
! --------------------------------------------------------------------------------------------------
!jref:start
   ERRSW   = SWDOWN - (FSA + FSR)
!   ERRSW   = SWDOWN - (SAV+SAG + FSRV+FSRG)
!   WRITE(*,*) "ERRSW =",ERRSW
   IF (ABS(ERRSW) > 0.01) THEN            ! w/m2
   WRITE(*,*) "VEGETATION!"
   WRITE(*,*) "SWDOWN*FVEG =",SWDOWN*FVEG
   WRITE(*,*) "FVEG*(SAV+SAG) =",FVEG*SAV + SAG
   WRITE(*,*) "FVEG*(FSRV +FSRG)=",FVEG*FSRV + FSRG
   WRITE(*,*) "GROUND!"
   WRITE(*,*) "(1-.FVEG)*SWDOWN =",(1.-FVEG)*SWDOWN
   WRITE(*,*) "(1.-FVEG)*SAG =",(1.-FVEG)*SAG
   WRITE(*,*) "(1.-FVEG)*FSRG=",(1.-FVEG)*FSRG
   WRITE(*,*) "FSRV   =",FSRV
   WRITE(*,*) "FSRG   =",FSRG
   WRITE(*,*) "FSR    =",FSR
   WRITE(*,*) "SAV    =",SAV
   WRITE(*,*) "SAG    =",SAG
   WRITE(*,*) "FSA    =",FSA
!jref:end   
      WRITE(message,*) 'ERRSW =',ERRSW
      call wrf_message(trim(message))
      call wrf_error_fatal("Stop IN Noah-MP")
   END IF

   ERRENG = SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL) +PAH
!   ERRENG = FVEG*SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL)
!   WRITE(*,*) "ERRENG =",ERRENG
   IF(ABS(ERRENG) > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG,' at i,j: ',ILOC,JLOC
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net solar:       ",FSA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net longwave:    ",FIRA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total sensible:  ",FSH
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Canopy evap:     ",FCEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Ground evap:     ",FGEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Transpiration:   ",FCTR
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total ground:    ",SSOIL
      call wrf_message(trim(message))
      WRITE(message,'(a17,4F10.4)') "Precip advected: ",PAH,PAHV,PAHG,PAHB
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Precip: ",PRCP
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Veg fraction: ",FVEG
      call wrf_message(trim(message))
      call wrf_error_fatal("Energy budget problem IN NOAHMP LSM")
   END IF

   IF (IST == 1) THEN                                       !soil
        END_WB = CANLIQ + CANICE + SNEQV + WA
        DO IZ = 1,NSOIL
          END_WB = END_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
        END DO
        ERRWAT = END_WB-BEG_WB-(PRCP-ECAN-ETRAN-EDIR-RUNSRF-RUNSUB)*DT

#ifndef WRF_HYDRO
        IF(ABS(ERRWAT) > 0.1) THEN
           if (ERRWAT > 0) then
              call wrf_message ('The model is gaININg water (ERRWAT is positive)')
           else
              call wrf_message('The model is losINg water (ERRWAT is negative)')
           endif
           write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
           call wrf_message(trim(message))
           WRITE(message, &
           '("    I      J     END_WB     BEG_WB       PRCP       ECAN       EDIR      ETRAN      RUNSRF     RUNSUB")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,9f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,ECAN*DT,&
                EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT,ZWT
           call wrf_message(trim(message))
           call wrf_error_fatal("Water budget problem IN NOAHMP LSM")
        END IF
#endif
   ELSE                 !KWM
      ERRWAT = 0.0      !KWM
   ENDIF

 END SUBROUTINE ERROR

!== begIN energy ===================================================================================

  SUBROUTINE ENERGY (parameters,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !IN
                     ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !IN
                     SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZREF   , & !IN
                     CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !IN
                     EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  , & !IN
                     ELAI   ,ESAI   ,FWET   ,FOLN   , UALB, BCS,        & !IN
                     FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !IN
                     QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,ILOC   , JLOC, & !IN
		                 Z0WRF  ,                                         &
                     IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !OUT
                     SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !OUT
                     TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !OUT
                     TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !OUT
                     PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                       & !OUT
                     TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !INOUT
                     SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !INOUT
                     ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , &   !INOUT
                     TAUSS  ,LAISUN ,LAISHA ,RB ,                     & !INOUT
!jref:start
                     QC     ,QSFC   ,PSFC   , & !IN 
                     T2MV   ,T2MB   ,FSRV   , &
                     FSRG   ,RSSUN  ,RSSHA  ,ALBSND  ,ALBSNI,BGAP   ,WGAP,TGV,TGB,&
                     Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV  ,CHB, EMISSI,PAH  ,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2, &
                     JULIAN, SWDOWN, PRCP, FB, GECROS1D )        
!jref:end                            

! --------------------------------------------------------------------------------------------------
! we use different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. We use 'tile' approach to compute turbulent fluxes, while we use modified two-
! stream to compute radiation transfer. Tile approach, assemblyINg vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree
! crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes IN vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves INto  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown IN
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / withIN the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                           , INTENT(IN)    :: ILOC
  INTEGER                           , INTENT(IN)    :: JLOC
  INTEGER                           , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                           , INTENT(IN)    :: VEGTYP !vegetation physiology type
  INTEGER                           , INTENT(IN)    :: IST    !surface type: 1->soil; 2->lake
  INTEGER                           , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                           , INTENT(IN)    :: NSOIL  !number of soil layers
  INTEGER                           , INTENT(IN)    :: ISNOW  !actual no. of snow layers
  REAL                              , INTENT(IN)    :: DT     !time step [sec]
  REAL                              , INTENT(IN)    :: QSNOW  !snowfall on the ground (mm/s)
  REAL                              , INTENT(IN)    :: RHOAIR !density air (kg/m3)
  REAL                              , INTENT(IN)    :: EAIR   !vapor pressure air (pa)
  REAL                              , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                              , INTENT(IN)    :: QAIR   !specific humidity (kg/kg)
  REAL                              , INTENT(IN)    :: SFCTMP !air temperature (k)
  REAL                              , INTENT(IN)    :: THAIR  !potential temperature (k)
  REAL                              , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                              , INTENT(IN)    :: UU     !wINd speed IN e-w dir (m/s)
  REAL                              , INTENT(IN)    :: VV     !wINd speed IN n-s dir (m/s)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  !INcomINg direct solar rad. (w/m2)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  !INcomINg diffuse solar rad. (w/m2)
  REAL                              , INTENT(IN)    :: COSZ   !cosINe solar zenith angle (0-1)
  REAL                              , INTENT(IN)    :: ELAI   !LAI adjusted for buryINg by snow
  REAL                              , INTENT(IN)    :: ESAI   !LAI adjusted for buryINg by snow
  REAL                              , INTENT(IN)    :: FWET   !fraction of canopy that is wet [-]
  REAL                              , INTENT(IN)    :: FVEG   !greeness vegetation fraction (-)
  REAL                              , INTENT(IN)    :: LAT    !latitude (radians)
  REAL                              , INTENT(IN)    :: CANLIQ !canopy-INtercepted liquid water (mm)
  REAL                              , INTENT(IN)    :: CANICE !canopy-INtercepted ice mass (mm)
  REAL                              , INTENT(IN)    :: FOLN   !foliage nitrogen (%)
  REAL                              , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                              , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)
  REAL                              , INTENT(IN)    :: IGS    !growINg season INdex (0=off, 1=on)
  REAL(r8)                              , INTENT(IN)    :: BCS     !bc IN snow (ng/g)
  REAL                              , INTENT(IN)    :: UALB   !Albedo of underlyINg surface
  REAL                              , INTENT(IN)    :: ZREF   !reference height (m)
  REAL                              , INTENT(IN)    :: TBOT   !bottom condition for soil temp. (k) 
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !depth of snow & soil layer-bottom [m]
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)

!jref:start; IN 
  REAL                              , INTENT(IN)    :: QC     !cloud water mixINg ratio
  REAL                              , INTENT(INOUT) :: QSFC   !mixINg ratio at lowest model layer
  REAL                              , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                              , INTENT(IN)    :: DX     !horisontal resolution
  REAL                              , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                              , INTENT(IN)    :: Q2     !mixINg ratio (kg/kg)
!jref:end

! OUTputs
  REAL                              , INTENT(OUT)   :: Z0WRF  !combINed z0 sent to coupled model
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  !phase change INdex [1-melt; 2-freeze]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV !partial volume ice [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV !partial volume liq. water [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  !effective porosity [m3/m3]
  REAL                              , INTENT(OUT)   :: FSNO   !snow cover fraction (-)
  REAL                              , INTENT(OUT)   :: QMELT  !snowmelt [mm/s]
  REAL                              , INTENT(OUT)   :: PONDING!poundINg at ground [mm]
  REAL                              , INTENT(OUT)   :: SAV    !solar rad. absorbed by veg. (w/m2)
  REAL                              , INTENT(OUT)   :: SAG    !solar rad. absorbed by ground (w/m2)
  REAL                              , INTENT(OUT)   :: FSA    !tot. absorbed solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSR    !tot. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: TAUX   !wINd stress: e-w (n/m2)
  REAL                              , INTENT(OUT)   :: TAUY   !wINd stress: n-s (n/m2)
  REAL                              , INTENT(OUT)   :: FIRA   !total net LW. rad (w/m2)   [+ to atm]
  REAL                              , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                              , INTENT(OUT)   :: FCEV   !canopy evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FGEV   !ground evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FCTR   !transpiration (w/m2)       [+ to atm]
  REAL                              , INTENT(OUT)   :: TRAD   !radiative temperature (k)
  REAL                              , INTENT(OUT)   :: T2M    !2 m height air temperature (k)
  REAL                              , INTENT(OUT)   :: PSN    !total photosyn. (umolco2/m2/s) [+]
  REAL                              , INTENT(OUT)   :: APAR   !total photosyn. active energy (w/m2)
  REAL                              , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL   , DIMENSION(       1:NSOIL), INTENT(OUT)   :: BTRANI !soil water transpiration factor (0-1)
  REAL                              , INTENT(OUT)   :: BTRAN  !soil water transpiration factor (0-1)
!  REAL                              , INTENT(OUT)   :: LATHEA !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(OUT)   :: FROZEN_GROUND ! used to defINe latent heat pathway
  LOGICAL                           , INTENT(OUT)   :: FROZEN_CANOPY ! used to defINe latent heat pathway

!jref:start  
  REAL                              , INTENT(OUT)   :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
!jref:end - OUT for debug  

!jref:start; OUTput
  REAL                              , INTENT(OUT)   :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                              , INTENT(OUT)   :: T2MB   !2-m air temperature over bare ground part [k]
  REAL                              , INTENT(OUT)   :: BGAP
  REAL                              , INTENT(OUT)   :: WGAP
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSNI   !snow albedo (diffuse)
!jref:end

! INput & OUTput
  REAL                              , INTENT(INOUT) :: TS     !surface temperature (k)
  REAL                              , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                              , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL                              , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL                              , INTENT(INOUT) :: SNEQV  !snow mass (mm)
  REAL                              , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow ice mass (kg/m2)
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow liq mass (kg/m2)
  REAL                              , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                              , INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL                              , INTENT(INOUT) :: ALBOLD !snow albedo at last time step(CLASS type)
  REAL                              , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                              , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                              , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                              , INTENT(INOUT) :: Q1
  REAL                              , INTENT(INOUT) :: RB     !leaf boundary layer resistance (s/m)
  REAL                              , INTENT(INOUT) :: LAISUN !sunlit leaf area INdex (m2/m2)
  REAL                              , INTENT(INOUT) :: LAISHA !shaded leaf area INdex (m2/m2)
!  REAL                                              :: Q2E
  REAL,                               INTENT(OUT)   :: EMISSI
  REAL,                               INTENT(OUT)   :: PAH    !precipitation advected heat - total (W/m2)

! local
  INTEGER                                           :: IZ     !do-loop INdex
  LOGICAL                                           :: VEG    !true if vegetated surface
  REAL                                              :: UR     !wINd speed at height ZLVL (m/s)
  REAL                                              :: ZLVL   !reference height (m)
  REAL                                              :: FSUN   !sunlit fraction of canopy [-]
 ! REAL                                             :: RB     !leaf boundary layer resistance (s/m)
  REAL                                              :: RSURF  !ground surface resistance (s/m)
  REAL                                              :: L_RSURF!Dry-layer thickness for computINg RSURF (Sakaguchi and Zeng, 2009)
  REAL                                              :: D_RSURF!Reduced vapor diffusivity IN soil for computINg RSURF (SZ09)
  REAL                                              :: BEVAP  !soil water evaporation factor (0- 1)
  REAL                                              :: MOL    !MonIN-Obukhov length (m)
  REAL                                              :: VAI    !sum of LAI  + stem area INdex [m2/m2]
  REAL                                              :: CWP    !canopy wINd extINction parameter
  REAL                                              :: ZPD    !zero plane displacement (m)
  REAL                                              :: Z0M    !z0 momentum (m)
  REAL                                              :: ZPDG   !zero plane displacement (m)
  REAL                                              :: Z0MG   !z0 momentum, ground (m)
  REAL                                              :: EMV    !vegetation emissivity
  REAL                                              :: EMG    !ground emissivity
  REAL                                              :: FIRE   !emitted IR (w/m2)

  REAL                                              :: PSNSUN !sunlit photosynthesis (umolco2/m2/s)
  REAL                                              :: PSNSHA !shaded photosynthesis (umolco2/m2/s)
!jref:start - for debug  
!  REAL                                              :: RSSUN  !sunlit stomatal resistance (s/m)
!  REAL                                              :: RSSHA  !shaded stomatal resistance (s/m)
!jref:end - for debug
  REAL                                              :: PARSUN !par absorbed per sunlit LAI (w/m2)
  REAL                                              :: PARSHA !par absorbed per shaded LAI (w/m2)

  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   !temporary used IN phase change
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  !heat capacity [j/m3/k]
  REAL                                              :: BDSNO  !bulk density of snow (kg/m3)
  REAL                                              :: FMELT  !meltINg factor for snow cover frac
  REAL                                              :: GX     !temporary variable
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: PHI    !light through water (w/m2)
!  REAL                                              :: GAMMA  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAV  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAG  !psychrometric constant (pa/k)
  REAL                                              :: PSI    !surface layer soil matrix potential (m)
  REAL                                              :: RHSUR  !raltive humidity IN surface soil/snow air space (-)

! temperature and fluxes over vegetated fraction

  REAL                                              :: TAUXV  !wINd stress: e-w dir [n/m2]
  REAL                                              :: TAUYV  !wINd stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
!jref:start  
  REAL,INTENT(OUT)                                  :: Q2V
  REAL,INTENT(OUT)                                  :: Q2B
  REAL,INTENT(OUT)                                  :: Q2E
!jref:end  
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                  :: TGV    !ground surface temp. [k]
  REAL                                              :: CMV    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHV    !sensible heat exchange coefficient

! temperature and fluxes over bare soil fraction

  REAL                                              :: TAUXB  !wINd stress: e-w dir [n/m2]
  REAL                                              :: TAUYB  !wINd stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                  :: TGB    !ground surface temp. [k]
  REAL                                              :: CMB    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHB    !sensible heat exchange coefficient
  REAL,INTENT(OUT)                                  :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                                  :: CHUC   !under canopy exchange coefficient
!jref:start  
  REAL,INTENT(OUT)                                  :: CHV2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL,INTENT(OUT)                                  :: CHB2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL                                  :: noahmpres

  REAL,                INTENT(IN)    :: JULIAN, SWDOWN, PRCP, FB
  REAL,DIMENSION(1:60),INTENT(INOUT) :: GECROS1D

!jref:end  

  REAL, PARAMETER                   :: MPE    = 1.E-6
  REAL, PARAMETER                   :: PSIWLT = -150.  !metric potential for wiltINg poINt (m)
  REAL, PARAMETER                   :: Z0     = 0.002  ! Bare-soil roughness length (m) (i.e., under the canopy)

! ---------------------------------------------------------------------------------------------------
! INitialize fluxes from veg. fraction

    TAUXV     = 0.    
    TAUYV     = 0.
    IRC       = 0.
    SHC       = 0.
    IRG       = 0.
    SHG       = 0.
    EVG       = 0.       
    EVC       = 0.
    TR        = 0.
    GHV       = 0.       
    PSNSUN    = 0.
    PSNSHA    = 0.
    T2MV      = 0.
    Q2V       = 0.
    CHV       = 0.
    CHLEAF    = 0.
    CHUC      = 0.
    CHV2      = 0.
    RB        = 0.

! wINd speed at reference height: ur >= 1

    UR = MAX( SQRT(UU**2.+VV**2.), 1. )

! vegetated or non-vegetated

    VAI = ELAI + ESAI
    VEG = .FALSE.
    IF(VAI > 0.) VEG = .TRUE.

! ground snow cover fraction [Niu and Yang, 2007, JGR]

     FSNO = 0.
     IF(SNOWH.GT.0.)  THEN
         BDSNO    = SNEQV / SNOWH
         FMELT    = (BDSNO/100.)**parameters%MFSNO
         FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
     ENDIF

! ground roughness length

     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * parameters%Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * parameters%Z0SNO
     END IF

! roughness length and displacement height

     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = parameters%Z0MVT
        ZPD  = 0.65 * parameters%HVT
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

! special case for urban

     IF (parameters%urban_flag) THEN
       Z0MG = parameters%Z0MVT
       ZPDG  = 0.65 * parameters%HVT
       Z0M  = Z0MG
       ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,parameters%HVT) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF
!     UR   = UR*LOG(ZLVL/Z0M)/LOG(10./Z0M)       !INput UR is at 10m

! canopy wINd absorption coeffcient

     CWP = parameters%CWPVT

! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !IN
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !IN
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & !IN
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !IN
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !OUT
                   FACT    )                              !OUT

! Solar radiation: absorbed & reflected by the ground and canopy

  CALL  RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !IN 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !IN
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !IN
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !IN
                   FVEG    ,ILOC    ,JLOC    ,NSNOW  ,ISNOW   , & !IN
                   BCS     ,UALB   ,SNICE    ,SNLIQ           , & !IN
                   ALBOLD  ,TAUSS   ,                            & !INOUT
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !OUT
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP     )  !OUT

! vegetation and ground emissivity

     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.-FSNO) + 1.0*FSNO
     ELSE
       EMG = parameters%EG(IST)*(1.-FSNO) + 1.0*FSNO
     END IF

! soil moisture factor controllINg stomatal resistance
   
     BTRAN = 0.

     IF(IST ==1 ) THEN
       DO IZ = 1, parameters%NROOT
          IF(OPT_BTR == 1) then                  ! Noah
            GX    = (SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
          END IF
          IF(OPT_BTR == 2) then                  ! CLM
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = (1.-PSI/PSIWLT)/(1.+parameters%PSISAT(IZ)/PSIWLT)
          END IF
          IF(OPT_BTR == 3) then                  ! SSiB
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI))) 
          END IF
       
          GX = MIN(1.,MAX(0.,GX))
          BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(parameters%NROOT)) * GX)
          BTRAN      = BTRAN + BTRANI(IZ)
       END DO
       BTRAN = MAX(MPE,BTRAN)

       BTRANI(1:parameters%NROOT) = BTRANI(1:parameters%NROOT)/BTRAN
     END IF

! soil surface resistance for ground evap.

     BEVAP = MAX(0.0,SH2O(1)/parameters%SMCMAX(1))
     IF(IST == 2) THEN
       RSURF = 1.          ! avoid beINg divided by 0
       RHSUR = 1.0
     ELSE

       IF(OPT_RSF == 1 .OR. OPT_RSF == 4) THEN
         ! RSURF based on Sakaguchi and Zeng, 2009
         ! takINg the "residual water content" to be the wiltINg poINt, 
         ! and correctINg the exponent on the D term (typo IN SZ09 ?)
         L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 ) 
         D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** (2.0+3.0/parameters%BEXP(1))
         RSURF = L_RSURF / D_RSURF
       ELSEIF(OPT_RSF == 2) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992) ! Older RSURF computations
       ELSEIF(OPT_RSF == 3) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil
       ENDIF

       IF(OPT_RSF == 4) THEN  ! AD: FSNO weighted; snow RSURF set IN MPTABLE v3.8
         RSURF = 1. / (FSNO * (1./parameters%RSURF_SNOW) + (1.-FSNO) * (1./max(RSURF, 0.001)))
       ENDIF

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
       PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))   
       RHSUR = FSNO + (1.-FSNO) * EXP(PSI*GRAV/(RW*TG)) 
     END IF

! urban - jref 
     IF (parameters%urban_flag .and. SNOWH == 0. ) THEN
        RSURF = 1.E6
     ENDIF

! set psychrometric constant

     IF (TV .GT. TFRZ) THEN           ! Barlage: add distINction between ground and 
        LATHEAV = HVAP                ! vegetation IN v3.6
	frozen_canopy = .false.
     ELSE
        LATHEAV = HSUB
	frozen_canopy = .true.
     END IF
     GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)

     IF (TG .GT. TFRZ) THEN
        LATHEAG = HVAP
	frozen_ground = .false.
     ELSE
        LATHEAG = HSUB
	frozen_ground = .true.
     END IF
     GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)

!     IF (SFCTMP .GT. TFRZ) THEN
!        LATHEA = HVAP
!     ELSE
!        LATHEA = HSUB
!     END IF
!     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

! Surface temperatures of the ground and canopy and energy fluxes

    IF (VEG .AND. FVEG > 0) THEN 
    TGV = TG
    CMV = CM
    CHV = CH
    CALL VEGE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !IN
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !IN
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !IN
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG   , & !IN
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !IN
                    ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !IN
                    Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO, & !IN
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !IN
                    RSURF   ,LATHEAV ,LATHEAG ,PARSUN  ,PARSHA  ,IGS     , & !IN
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !IN
                    RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV  ,PAHG  , & !IN
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & !INOUT
                    CHV     ,DX      ,DZ8W    ,                   & !INOUT
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !OUT
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & !OUT
                    T2MV    ,PSNSUN  ,PSNSHA  ,                   & !OUT
!jref:start
                    QC      ,QSFC    ,PSFC    , & !IN
                    Q2V     ,CHV2, CHLEAF, CHUC, &
                    SH2O,JULIAN, SWDOWN, PRCP, FB, FSR, GECROS1D)      ! Gecros 
!jref:end                            
    END IF

    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !IN
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !IN
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !IN
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,FSNO,          & !IN
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEAG  , & !IN
                    GAMMAG   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !IN
                    TGB     ,CMB     ,CHB     ,                   & !INOUT
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !OUT
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & !OUT
!jref:start
                    QC      ,QSFC    ,PSFC    , & !IN
                    SFCPRS  ,Q2B,   CHB2)                          !IN 
!jref:end                            

!energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG 
!energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
!energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (VEG .AND. FVEG > 0) THEN 
        TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
        TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
        FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
        FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
	PAH   = FVEG * PAHG      + (1.0 - FVEG) * PAHB   + PAHV
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
	Z0WRF = Z0M
    ELSE
        TAUX  = TAUXB
        TAUY  = TAUYB
        FIRA  = IRB
        FSH   = SHB
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        T2M   = T2MB
        FCEV  = 0.
        FCTR  = 0.
	PAH   = PAHB
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
	Z0WRF = Z0MG
    END IF

    FIRE = LWDN + FIRA

    IF(FIRE <=0.) THEN
       WRITE(6,*) 'emitted longwave <0; skIN T may be wrong due to INconsistent'
       WRITE(6,*) 'INput of SHDFAC with LAI'
       WRITE(6,*) ILOC, JLOC, 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       WRITE(6,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       call wrf_error_fatal("STOP IN Noah-MP")
    END IF

    ! Compute a net emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
         (1-FVEG) * EMG

    ! When we're computINg a TRAD, subtract from the emitted IR the
    ! reflected portion of the INcomINg LWDN, so we're just
    ! considerINg the IR origINatINg IN the canopy/ground system.
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

    ! Old TRAD calculation not takINg INto account Emissivity:
    ! TRAD = (FIRE/SB)**0.25

    APAR = PARSUN*LAISUN + PARSHA*LAISHA
    PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA

! 3L snow & 4L soil temperatures

    CALL TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !IN
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !IN
                  SAG     ,DT      ,SNOWH   ,DZSNSO  , & !IN
                  TG      ,ILOC    ,JLOC    ,                   & !IN
                  STC     )                                       !INOUT

! adjustINg snow surface temperature
     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TGV = TFRZ
        TGB = TFRZ
          IF (VEG .AND. FVEG > 0) THEN
             TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
             TS    = FVEG * TV        + (1.0 - FVEG) * TGB
          ELSE
             TG    = TGB
             TS    = TGB
          END IF
      END IF
     END IF

! Energy released or consumed by snow & frozen soil

 CALL PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !IN
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !IN
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !INOUT
                   SMC     ,SH2O    ,                            & !INOUT
                   QMELT   ,IMELT   ,PONDING )                     !OUT


  END SUBROUTINE ENERGY

!== begIN thermoprop ===============================================================================

  SUBROUTINE THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !IN
                         DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !IN
                         SMC     ,SH2O    ,TG      ,STC     ,UR      , & !IN
                         LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !IN
                         DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !OUT
                         FACT    )                                       !OUT
! ------------------------------------------------------------------------------------------------- 
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN)  :: NSOIL   !number of soil layers
  INTEGER                        , INTENT(IN)  :: NSNOW   !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)  :: ISNOW   !actual no. of snow layers
  INTEGER                        , INTENT(IN)  :: IST     !surface type
  REAL                           , INTENT(IN)  :: DT      !time step [s]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  !thickness of snow/soil layers [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SMC     !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SH2O    !liquid soil moisture [m3/m3]
  REAL                           , INTENT(IN)  :: SNOWH   !snow height [m]
  REAL,                            INTENT(IN)  :: TG      !surface temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC     !snow/soil/lake temp. (k)
  REAL,                            INTENT(IN)  :: UR      !wINd speed at ZLVL (m/s)
  REAL,                            INTENT(IN)  :: LAT     !latitude (radians)
  REAL,                            INTENT(IN)  :: Z0M     !roughness length (m)
  REAL,                            INTENT(IN)  :: ZLVL    !reference height (m)
  INTEGER                        , INTENT(IN)  :: VEGTYP  !vegtyp type

! OUTputs
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   !heat capacity [j/m3/k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   !effective porosity [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    !computINg energy for phase change
! --------------------------------------------------------------------------------------------------
! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   !snow thermal conductivity (j/m3/k)
  REAL, DIMENSION(       1:NSOIL)              :: SICE    !soil ice content
! --------------------------------------------------------------------------------------------------

! compute snow thermal conductivity and heat capacity

    CALL CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !IN
                TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !OUT

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO

! compute soil thermal properties

    DO  IZ = 1, NSOIL
       SICE(IZ)  = SMC(IZ) - SH2O(IZ)
       HCPCT(IZ) = SH2O(IZ)*CWAT + (1.0-parameters%SMCMAX(IZ))*parameters%CSOIL &
                + (parameters%SMCMAX(IZ)-SMC(IZ))*CPAIR + SICE(IZ)*CICE
       CALL TDFCND (parameters,IZ,DF(IZ), SMC(IZ), SH2O(IZ))
    END DO
       
    IF ( parameters%urban_flag ) THEN
       DO IZ = 1,NSOIL
         DF(IZ) = 3.24
       END DO
    ENDIF

! heat flux reduction effect from the overlyINg green canopy, adapted from 
! section 2.1.2 of Peters-Lidard et al. (1997, JGR, VOL 102(D4)).
! not IN use because of the separation of the canopy layer from the ground.
! but this may represent the effects of leaf litter (Niu comments)
!       DF1 = DF1 * EXP (SBETA * SHDFAC)

! compute lake thermal properties 
! (no consideration of turbulent mixINg for this version)

    IF(IST == 2) THEN
       DO IZ = 1, NSOIL 
         IF(STC(IZ) > TFRZ) THEN
            HCPCT(IZ) = CWAT
            DF(IZ)    = TKWAT  !+ KEDDY * CWAT 
         ELSE
            HCPCT(IZ) = CICE
            DF(IZ)    = TKICE 
         END IF
       END DO
    END IF

! combINe a temporary variable used for meltINg/freezINg of snow and frozen soil

    DO IZ = ISNOW+1,NSOIL
     FACT(IZ) = DT/(HCPCT(IZ)*DZSNSO(IZ))
    END DO

! snow/soil INterface

    IF(ISNOW == 0) THEN
       DF(1) = (DF(1)*DZSNSO(1)+0.35*SNOWH)      / (SNOWH    +DZSNSO(1)) 
    ELSE
       DF(1) = (DF(1)*DZSNSO(1)+DF(0)*DZSNSO(0)) / (DZSNSO(0)+DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP

!== begIN csnow ====================================================================================

  SUBROUTINE CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !IN
                    TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !OUT
! --------------------------------------------------------------------------------------------------
! Snow bulk density,volumetric capacity, and thermal conductivity
!---------------------------------------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! INputs

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                          INTENT(IN) :: ISNOW  !number of snow layers (-)            
  INTEGER                        ,  INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        ,  INTENT(IN) :: NSOIL  !number of soil layers
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2) 
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]

! OUTputs

  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: CVSNO  !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: TKSNO  !thermal conductivity (w/m/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE  !effective porosity [m3/m3]

! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0) :: BDSNOI  !bulk density of snow(kg/m3)

!---------------------------------------------------------------------------------------------------
! thermal capacity of snow

  DO IZ = ISNOW+1, 0
      SNICEV(IZ)   = MIN(1., SNICE(IZ)/(DZSNSO(IZ)*DENICE) )
      EPORE(IZ)    = 1. - SNICEV(IZ)
      SNLIQV(IZ)   = MIN(EPORE(IZ),SNLIQ(IZ)/(DZSNSO(IZ)*DENH2O))
  ENDDO

  DO IZ = ISNOW+1, 0
      BDSNOI(IZ) = (SNICE(IZ)+SNLIQ(IZ))/DZSNSO(IZ)
      CVSNO(IZ) = CICE*SNICEV(IZ)+CWAT*SNLIQV(IZ)
!      CVSNO(IZ) = 0.525E06                          ! constant
  enddo

! thermal conductivity of snow

  DO IZ = ISNOW+1, 0
     TKSNO(IZ) = 3.2217E-6*BDSNOI(IZ)**2.           ! Stieglitz(yen,1965)
!    TKSNO(IZ) = 2E-2+2.5E-6*BDSNOI(IZ)*BDSNOI(IZ)   ! Anderson, 1976
!    TKSNO(IZ) = 0.35                                ! constant
!    TKSNO(IZ) = 2.576E-6*BDSNOI(IZ)**2. + 0.074    ! Verseghy (1991)
!    TKSNO(IZ) = 2.22*(BDSNOI(IZ)/1000.)**1.88      ! Douvill(Yen, 1981)
  ENDDO

  END SUBROUTINE CSNOW

!== begIN tdfcnd ===================================================================================

  SUBROUTINE TDFCND (parameters, ISOIL, DF, SMC, SH2O)
! --------------------------------------------------------------------------------------------------
! Calculate thermal diffusivity and conductivity of the soil.
! Peters-Lidard approach (Peters-Lidard et al., 1998)
! --------------------------------------------------------------------------------------------------
! Code history:
! June 2001 changes: frozen soil condition.
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER, INTENT(IN)    :: ISOIL  ! soil layer
    REAL, INTENT(IN)       :: SMC    ! total soil water
    REAL, INTENT(IN)       :: SH2O   ! liq. soil water
    REAL, INTENT(OUT)      :: DF     ! thermal diffusivity

! local variables
    REAL  :: AKE
    REAL  :: GAMMD
    REAL  :: THKDRY
    REAL  :: THKO     ! thermal conductivity for other soil components         
    REAL  :: THKQTZ   ! thermal conductivity for quartz
    REAL  :: THKSAT   ! 
    REAL  :: THKS     ! thermal conductivity for the solids
    REAL  :: THKW     ! water thermal conductivity
    REAL  :: SATRATIO
    REAL  :: XU
    REAL  :: XUNFROZ
! --------------------------------------------------------------------------------------------------
! We now get quartz as an INput argument (set IN rOUTINe redprm):
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,
!     &             0.35, 0.60, 0.40, 0.82/
! --------------------------------------------------------------------------------------------------
! If the soil has any moisture content compute a partial sum/product
! otherwise use a constant value which works well with most soils
! --------------------------------------------------------------------------------------------------
!  QUARTZ ....QUARTZ CONTENT (SOIL TYPE DEPENDENT)
! --------------------------------------------------------------------------------------------------
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).

!                                  PABLO GRUNMANN, 08/17/98
! Refs.:
!      Farouki, O.T.,1986: Thermal properties of soils. Series on Rock
!              and Soil Mechanics, Vol. 11, Trans Tech, 136 pp.
!      Johansen, O., 1975: Thermal conductivity of soils. PH.D. Thesis,
!              University of Trondheim,
!      Peters-Lidard, C. D., et al., 1998: The effect of soil thermal
!              conductivity parameterization on surface energy fluxes
!              and temperatures. Journal of The Atmospheric Sciences,
!              Vol. 55, pp. 1209-1224.
! --------------------------------------------------------------------------------------------------
! NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
! PARAMETERS  W/(M.K)
    SATRATIO = SMC / parameters%SMCMAX(ISOIL)
    THKW = 0.57
!      IF (QUARTZ .LE. 0.2) THKO = 3.0
    THKO = 2.0
! SOLIDS' CONDUCTIVITY
! QUARTZ' CONDUCTIVITY
    THKQTZ = 7.7

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
    THKS = (THKQTZ ** parameters%QUARTZ(ISOIL))* (THKO ** (1. - parameters%QUARTZ(ISOIL)))

! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
    XUNFROZ = 1.0                       ! Prevent divide by zero (suggested by D. Mocko)
    IF(SMC > 0.) XUNFROZ = SH2O / SMC
! SATURATED THERMAL CONDUCTIVITY
    XU = XUNFROZ * parameters%SMCMAX(ISOIL)

! DRY DENSITY IN KG/M3
    THKSAT = THKS ** (1. - parameters%SMCMAX(ISOIL))* TKICE ** (parameters%SMCMAX(ISOIL) - XU)* THKW **   &
         (XU)

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
    GAMMD = (1. - parameters%SMCMAX(ISOIL))*2700.

    THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)
! FROZEN
    IF ( (SH2O + 0.0005) <  SMC ) THEN
       AKE = SATRATIO
! UNFROZEN
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)
    ELSE

! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

       IF ( SATRATIO >  0.1 ) THEN

          AKE = LOG10 (SATRATIO) + 1.0

! USE K = KDRY
       ELSE

          AKE = 0.0
       END IF
!  THERMAL CONDUCTIVITY

    END IF

    DF = AKE * (THKSAT - THKDRY) + THKDRY


  end SUBROUTINE TDFCND

!== begIN radiation ================================================================================

  SUBROUTINE RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !IN
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !IN
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !IN
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !IN
                        FVEG    ,ILOC    ,JLOC    ,NSNOW  ,ISNOW   , & !IN
                        BCS     ,UALB   ,SNICE    ,SNLIQ           , & !IN
                        ALBOLD  ,TAUSS   ,                            & !INOUT
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !OUT
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP    )   !OUT
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP !vegetation type
  INTEGER, INTENT(IN)                  :: IST    !surface type
  INTEGER, INTENT(IN)                  :: ICE    !ice (ice = 1)
  INTEGER, INTENT(IN)                  :: NSOIL  !number of soil layers
  INTEGER,INTENT(IN)                   :: ISNOW   !actual no. of snow layers
  INTEGER,INTENT(IN)                   :: NSNOW  !maximum no. of snow layers   
  REAL, INTENT(IN)                     :: DT     !time step [s]
  REAL, INTENT(IN)                     :: QSNOW  !snowfall (mm/s)
  REAL, INTENT(IN)                     :: SNEQVO !snow mass at last time step(mm)
  REAL, INTENT(IN)                     :: SNEQV  !snow mass (mm)
  REAL, INTENT(IN)                     :: SNOWH  !snow height (mm)
  REAL, INTENT(IN)                     :: COSZ   !cosINe solar zenith angle (0-1)
  REAL, INTENT(IN)                     :: TG     !ground temperature (k)
  REAL, INTENT(IN)                     :: TV     !vegetation temperature (k)
  REAL, INTENT(IN)                     :: ELAI   !LAI, one-sided, adjusted for buryINg by snow
  REAL, INTENT(IN)                     :: ESAI   !SAI, one-sided, adjusted for buryINg by snow
  REAL, INTENT(IN)                     :: FWET   !fraction of canopy that is wet
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !volumetric soil water [m3/m3]
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  !INcomINg direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  !INcomINg diffuse solar radiation (w/m2)
  REAL, INTENT(IN)                     :: FSNO   !snow cover fraction (-)
  REAL, INTENT(IN)                     :: FVEG   !green vegetation fraction [0.0-1.0]     
  REAL(r8),INTENT(IN)                      :: BCS     !bc IN snow (ng/g)
  REAL,INTENT(IN)                      :: UALB   !Albedo of underlyINg surface
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2)
! INOUT
  REAL,                  INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT) :: TAUSS  !non-dimensional snow age.

! OUTput
  REAL, INTENT(OUT)                    :: FSUN   !sunlit fraction of canopy (-)
  REAL, INTENT(OUT)                    :: LAISUN !sunlit leaf area (-)
  REAL, INTENT(OUT)                    :: LAISHA !shaded leaf area (-)
  REAL, INTENT(OUT)                    :: PARSUN !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                    :: PARSHA !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                    :: SAV    !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                    :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                    :: FSA    !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSR    !total reflected solar radiation (w/m2)

!jref:start  
  REAL, INTENT(OUT)                    :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: BGAP
  REAL, INTENT(OUT)                    :: WGAP
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSNI   !snow albedo (diffuse)
!jref:end  

! local
  REAL                                 :: FAGE   !snow age function (0 - new snow)
  REAL, DIMENSION(1:2)                 :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:2)                 :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:2)                 :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTII   !down diffuse flux below veg (per unit dif flux)
!jref:start  
  REAL, DIMENSION(1:2)                 :: FREVI
  REAL, DIMENSION(1:2)                 :: FREVD
  REAL, DIMENSION(1:2)                 :: FREGI
  REAL, DIMENSION(1:2)                 :: FREGD
!jref:end

  REAL                                 :: FSHA   !shaded fraction of canopy
  REAL                                 :: VAI    !total LAI + stem area INdex, one sided

  REAL,PARAMETER :: MPE = 1.E-6
  LOGICAL VEG  !true: vegetated for surface temperature calculation

! --------------------------------------------------------------------------------------------------

! surface abeldo

   CALL ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !IN
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !IN
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !IN
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !IN
                ILOC   ,JLOC   , NSNOW ,ISNOW  ,BCS    , & !IN 
                UALB   ,SNICE  ,SNLIQ                  , & !IN                
                ALBOLD ,TAUSS                          , & !INOUT
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !OUT
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !)   !OUT
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & !INOUT
                WGAP   ,ALBSND ,ALBSNI )

! surface radiation

     FSHA = 1.-FSUN
     LAISUN = ELAI*FSUN
     LAISHA = ELAI*FSHA
     VAI = ELAI+ ESAI
     IF (VAI .GT. 0.) THEN
        VEG = .TRUE.
     ELSE
        VEG = .FALSE.
     END IF

   CALL SURRAD (parameters,MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !IN
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !IN
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !IN
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !IN
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !OUT
                FSR    ,                                 & !OUT
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !INOUT
                FSRG)

  END SUBROUTINE RADIATION

!== begIN albedo ===================================================================================

  SUBROUTINE ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !IN
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !IN
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !IN
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !IN
                     ILOC   ,JLOC   , NSNOW ,ISNOW  ,BCS    , & !IN
                     UALB   ,SNICE  ,SNLIQ                  , & !IN
                     ALBOLD ,TAUSS                          , & !INOUT
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !OUT
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !OUT
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & !OUT
                     WGAP   ,ALBSND ,ALBSNI )

! --------------------------------------------------------------------------------------------------
! surface albedos. also fluxes (per unit INcomINg direct and diffuse
! radiation) reflected, transmitted, and absorbed by vegetation.
! also sunlit fraction of the canopy.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: VEGTYP !vegetation type
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ICE    !ice (ice = 1)

  REAL,                     INTENT(IN)  :: DT     !time step [sec]
  REAL,                     INTENT(IN)  :: QSNOW  !snowfall
  REAL,                     INTENT(IN)  :: COSZ   !cosINe solar zenith angle for next time step
  REAL,                     INTENT(IN)  :: SNOWH  !snow height (mm)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: TV     !vegetation temperature (k)
  REAL,                     INTENT(IN)  :: ELAI   !LAI, one-sided, adjusted for buryINg by snow
  REAL,                     INTENT(IN)  :: ESAI   !SAI, one-sided, adjusted for buryINg by snow
  REAL,                     INTENT(IN)  :: FSNO   !fraction of grid covered by snow
  REAL,                     INTENT(IN)  :: FWET   !fraction of canopy that is wet
  REAL,                     INTENT(IN)  :: SNEQVO !snow mass at last time step(mm)
  REAL,                     INTENT(IN)  :: SNEQV  !snow mass (mm)
  REAL,                     INTENT(IN)  :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water (m3/m3)
  INTEGER,INTENT(IN) :: ISNOW   !actual no. of snow layers
  INTEGER,INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  REAL(r8),INTENT(IN) :: BCS     !bc IN snow (ng/g)
  REAL,INTENT(IN) :: UALB   !Albedo of underlyINg surface
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2)
  
  
! INOUT
  REAL,                  INTENT(INOUT)  :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT)  :: TAUSS  !non-dimensional snow age

! OUTput
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL,                     INTENT(OUT) :: FSUN   !sunlit fraction of canopy (-)
!jref:start
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGI
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
!jref:end

! ------------------------------------------------------------------------
! ------------------------ local variables -------------------------------
! local
  REAL                 :: FAGE     !snow age function
  REAL                 :: ALB
  INTEGER              :: IB       !INdices
  INTEGER              :: NBAND    !number of solar radiation wave bands
  INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

  REAL                 :: WL       !fraction of LAI+SAI that is LAI
  REAL                 :: WS       !fraction of LAI+SAI that is SAI
  REAL                 :: MPE      !prevents overflow for division by zero

  REAL, DIMENSION(1:2) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: FTDI     !down direct flux below veg per unit dif flux = 0
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSNI   !snow albedo (diffuse)

  REAL                 :: VAI      !ELAI+ESAI
  REAL                 :: GDIR     !average projected leaf/stem area IN solar direction
  REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area

! --------------------------------------------------------------------------------------------------

  NBAND = 2
  MPE = 1.E-06
  BGAP = 0.
  WGAP = 0.

! INitialize OUTput because solar radiation only done if COSZ > 0

  DO IB = 1, NBAND
    ALBD(IB) = 0.
    ALBI(IB) = 0.
    ALBGRD(IB) = 0.
    ALBGRI(IB) = 0.
    ALBSND(IB) = 0.
    ALBSNI(IB) = 0.
    FABD(IB) = 0.
    FABI(IB) = 0.
    FTDD(IB) = 0.
    FTID(IB) = 0.
    FTII(IB) = 0.
    IF (IB.EQ.1) FSUN = 0.
  END DO

  IF(COSZ <= 0) GOTO 100

! weight reflectance/transmittance by LAI and SAI

  DO IB = 1, NBAND
    VAI = ELAI + ESAI
    WL  = ELAI / MAX(VAI,MPE)
    WS  = ESAI / MAX(VAI,MPE)
    RHO(IB) = MAX(parameters%RHOL(IB)*WL+parameters%RHOS(IB)*WS, MPE)
    TAU(IB) = MAX(parameters%TAUL(IB)*WL+parameters%TAUS(IB)*WS, MPE)
  END DO

! snow age

   CALL SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: only if COSZ > 0 and FSNO > 0

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (parameters,NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
  END IF 
  IF(OPT_ALB == 3) THEN
       CALL SNOWALB_SNICAR_DIR (parameters,NBAND,NSNOW,ISNOW,COSZ,BCS,UALB,SNICE,SNLIQ,SNEQV,ALBSND,ALBSNI)
  END IF
 ! IF(OPT_ALB == 4) THEN
  !     CALL SNOWALB_SNICAR_IDR (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
!  END IF


! ground surface albedo

  CALL GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !IN
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !IN
                  TG      ,ILOC    ,JLOC    ,                   & !IN
                  ALBGRD  ,ALBGRI  )                              !OUT

! loop over NBAND wavebands to calculate surface albedos and solar
! fluxes for unit INcomINg direct (IC=0) and diffuse flux (IC=1)

  DO IB = 1, NBAND
      IC = 0      ! direct
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !IN
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !IN
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !IN
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &!)   !OUT
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      ! diffuse
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !IN
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !IN
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !IN
                      FABI   ,ALBI    ,FTDI    ,FTII    ,GDIR   , & !)   !OUT
                      FREVI  ,FREGI   ,BGAP    ,WGAP)

  END DO

! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.

  EXT = GDIR/COSZ * SQRT(1.-RHO(1)-TAU(1))
  FSUN = (1.-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
  EXT = FSUN

  IF (EXT .LT. 0.01) THEN
     WL = 0.
  ELSE
     WL = EXT 
  END IF
  FSUN = WL

100 CONTINUE

  END SUBROUTINE ALBEDO

!== begIN surrad ===================================================================================

  SUBROUTINE SURRAD (parameters,MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & !IN
                     LAISUN  ,LAISHA  ,SOLAD   ,SOLAI   ,FABD    , & !IN
                     FABI    ,FTDD    ,FTID    ,FTII    ,ALBGRD  , & !IN
                     ALBGRI  ,ALBD    ,ALBI    ,ILOC    ,JLOC    , & !IN
                     PARSUN  ,PARSHA  ,SAV     ,SAG     ,FSA     , & !OUT
                     FSR     , & !)                                       !OUT
                     FREVI   ,FREVD   ,FREGD   ,FREGI   ,FSRV    , &
                     FSRG) !INOUT

! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER, INTENT(IN)              :: ILOC
  INTEGER, INTENT(IN)              :: JLOC
  REAL, INTENT(IN)                 :: MPE     !prevents underflow errors if division by zero

  REAL, INTENT(IN)                 :: FSUN    !sunlit fraction of canopy
  REAL, INTENT(IN)                 :: FSHA    !shaded fraction of canopy
  REAL, INTENT(IN)                 :: ELAI    !leaf area, one-sided
  REAL, INTENT(IN)                 :: VAI     !leaf + stem area, one-sided
  REAL, INTENT(IN)                 :: LAISUN  !sunlit leaf area INdex, one-sided
  REAL, INTENT(IN)                 :: LAISHA  !shaded leaf area INdex, one-sided

  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAD   !INcomINg direct solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAI   !INcomINg diffuse solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABD    !flux abs by veg (per unit INcomINg direct flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABI    !flux abs by veg (per unit INcomINg diffuse flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTDD    !down dir flux below veg (per INcomINg dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTID    !down dif flux below veg (per INcomINg dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTII    !down dif flux below veg (per INcomINg dif flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRD  !ground albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRI  !ground albedo (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBD    !overall surface albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBI    !overall surface albedo (diffuse)

  REAL, DIMENSION(1:2), INTENT(IN) :: FREVD    !overall surface albedo veg (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREVI    !overall surface albedo veg (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGD    !overall surface albedo grd (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGI    !overall surface albedo grd (diffuse)

! OUTput

  REAL, INTENT(OUT)                :: PARSUN  !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                :: PARSHA  !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                :: SAV     !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                :: SAG     !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                :: FSA     !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSR     !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSRV    !reflected solar radiation by vegetation
  REAL, INTENT(OUT)                :: FSRG    !reflected solar radiation by ground

! ------------------------ local variables ----------------------------------------------------
  INTEGER                          :: IB      !waveband number (1=vis, 2=nir)
  INTEGER                          :: NBAND   !number of solar radiation waveband classes

  REAL                             :: ABS     !absorbed solar radiation (w/m2)
  REAL                             :: RNIR    !reflected solar radiation [nir] (w/m2)
  REAL                             :: RVIS    !reflected solar radiation [vis] (w/m2)
  REAL                             :: LAIFRA  !leaf area fraction of canopy
  REAL                             :: TRD     !transmitted solar radiation: direct (w/m2)
  REAL                             :: TRI     !transmitted solar radiation: diffuse (w/m2)
  REAL, DIMENSION(1:2)             :: CAD     !direct beam absorbed by canopy (w/m2)
  REAL, DIMENSION(1:2)             :: CAI     !diffuse radiation absorbed by canopy (w/m2)
! ---------------------------------------------------------------------------------------------
   NBAND = 2

! zero summed solar fluxes

    SAG = 0.
    SAV = 0.
    FSA = 0.

! loop over nband wavebands

  DO IB = 1, NBAND

! absorbed by canopy

    CAD(IB) = SOLAD(IB)*FABD(IB)    
    CAI(IB) = SOLAI(IB)*FABI(IB)
    SAV     = SAV + CAD(IB) + CAI(IB)
    FSA     = FSA + CAD(IB) + CAI(IB)
 
! transmitted solar fluxes INcident on ground

    TRD = SOLAD(IB)*FTDD(IB)
    TRI = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)

! solar radiation absorbed by ground surface

    ABS = TRD*(1.-ALBGRD(IB)) + TRI*(1.-ALBGRI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
  END DO

! partition visible canopy absorption to sunlit and shaded fractions
! to get average absorbed par for sunlit and shaded leaves

     LAIFRA = ELAI / MAX(VAI,MPE)
     IF (FSUN .GT. 0.) THEN
        PARSUN = (CAD(1)+FSUN*CAI(1)) * LAIFRA / MAX(LAISUN,MPE)
        PARSHA = (FSHA*CAI(1))*LAIFRA / MAX(LAISHA,MPE)
     ELSE
        PARSUN = 0.
        PARSHA = (CAD(1)+CAI(1))*LAIFRA /MAX(LAISHA,MPE)
     ENDIF

! reflected solar radiation

     RVIS = ALBD(1)*SOLAD(1) + ALBI(1)*SOLAI(1)
     RNIR = ALBD(2)*SOLAD(2) + ALBI(2)*SOLAI(2)
     FSR  = RVIS + RNIR

! reflected solar radiation of veg. and ground (combINed ground)
     FSRV = FREVD(1)*SOLAD(1)+FREVI(1)*SOLAI(1)+FREVD(2)*SOLAD(2)+FREVI(2)*SOLAI(2)
     FSRG = FREGD(1)*SOLAD(1)+FREGI(1)*SOLAI(1)+FREGD(2)*SOLAD(2)+FREGI(2)*SOLAI(2)


  END SUBROUTINE SURRAD

!== begIN snow_age =================================================================================

  SUBROUTINE SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ code history ------------------------------------------------------------
! from BATS
! ------------------------ INput/OUTput variables --------------------------------------------------
!INput
  type (noahmp_parameters), INTENT(IN) :: parameters
   REAL, INTENT(IN) :: DT        !maIN time step (s)
   REAL, INTENT(IN) :: TG        !ground temperature (k)
   REAL, INTENT(IN) :: SNEQVO    !snow mass at last time step(mm)
   REAL, INTENT(IN) :: SNEQV     !snow water per unit ground area (mm)

!OUTput
   REAL, INTENT(OUT) :: FAGE     !snow age

!INput/OUTput
   REAL, INTENT(INOUT) :: TAUSS      !non-dimensional snow age
!local
   REAL            :: TAGE       !total agINg effects
   REAL            :: AGE1       !effects of graIN growth due to vapor diffusion
   REAL            :: AGE2       !effects of graIN growth at freezINg of melt water
   REAL            :: AGE3       !effects of soot
   REAL            :: DELA       !temporary variable
   REAL            :: SGE        !temporary variable
   REAL            :: DELS       !temporary variable
   REAL            :: DELA0      !temporary variable
   REAL            :: ARG        !temporary variable
! See Yang et al. (1997) J.of Climate for detail.
!---------------------------------------------------------------------------------------------------

   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.
   ELSE
          DELA0 = DT/parameters%TAU0
          ARG   = parameters%GRAIN_GROWTH*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,parameters%EXTRA_GROWTH*ARG))
          AGE3  = parameters%DIRT_SOOT
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / parameters%SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE

!== begIN snowalb_bats =============================================================================

  SUBROUTINE SNOWALB_BATS (parameters,NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: COSZ    !cosINe solar zenith angle
  REAL,INTENT(IN) :: FSNO    !snow cover fraction (-)
  REAL,INTENT(IN) :: FAGE    !snow age correction

! OUTput

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

  REAL :: FZEN                 !zenith angle correction
  REAL :: CF1                  !temperary variable
  REAL :: SL2                  !2.*SL
  REAL :: SL1                  !1/SL
  REAL :: SL                   !adjustable parameter
!  REAL, PARAMETER :: C1 = 0.2  !default IN BATS 
!  REAL, PARAMETER :: C2 = 0.5  !default IN BATS
!  REAL, PARAMETER :: C1 = 0.2 * 2. ! double the default to match Sleepers River's
!  REAL, PARAMETER :: C2 = 0.5 * 2. ! snow surface albedo (double agINg effects)
! ---------------------------------------------------------------------------------------------
! zero albedos for all poINts

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

        SL=parameters%BATS_COSZ
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=parameters%BATS_VIS_NEW*(1.-parameters%BATS_VIS_AGE*FAGE)         
        ALBSNI(2)=parameters%BATS_NIR_NEW*(1.-parameters%BATS_NIR_AGE*FAGE)        

        ALBSND(1)=ALBSNI(1)+parameters%BATS_VIS_DIR*FZEN*(1.-ALBSNI(1))    !  vis direct
        ALBSND(2)=ALBSNI(2)+parameters%BATS_VIS_DIR*FZEN*(1.-ALBSNI(2))    !  nir direct

  END SUBROUTINE SNOWALB_BATS

!== begIN snowalb_class ============================================================================

  SUBROUTINE SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,INTENT(IN) :: ILOC !grid INdex
  INTEGER,INTENT(IN) :: JLOC !grid INdex
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: QSNOW     !snowfall (mm/s)
  REAL,INTENT(IN) :: DT        !time step (sec)
  REAL,INTENT(IN) :: ALBOLD    !snow albedo at last time step

! IN & OUT

  REAL,                INTENT(INOUT) :: ALB        ! 
! OUTput

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

! ---------------------------------------------------------------------------------------------
! zero albedos for all poINts

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.)

! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
! here assume 1cm snow depth will fully cover the old snow

         IF (QSNOW > 0.) then
           ALB = ALB + MIN(QSNOW,parameters%SWEMX/DT) * (0.84-ALB)/(parameters%SWEMX/DT)
         ENDIF

         ALBSNI(1)= ALB         ! vis diffuse
         ALBSNI(2)= ALB         ! nir diffuse
         ALBSND(1)= ALB         ! vis direct
         ALBSND(2)= ALB         ! nir direct

  END SUBROUTINE SNOWALB_CLASS

!== begIN snowalb_snicar_dir =============================================================================

  SUBROUTINE SNOWALB_SNICAR_DIR (parameters,NBAND,NSNOW,ISNOW,COSZ,BCS,UALB,SNICE,SNLIQ,SNEQV,ALBSND,ALBSNI)

! --------------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput

   type (noahmp_parameters), INTENT(IN) :: parameters
   INTEGER,INTENT(IN) :: NBAND  !number of waveband classes=numrad
   INTEGER,INTENT(IN) :: ISNOW   !actual no. of snow layers
   INTEGER,INTENT(IN) :: NSNOW  !maximum no. of snow layers        
   REAL,INTENT(IN) :: COSZ    !cosINe solar zenith angle
   REAL(r8),INTENT(IN) :: BCS     !bc IN snow (ng/g)
   REAL,INTENT(IN) :: UALB   !Albedo of underlyINg surface
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2)
   REAL, INTENT(IN) :: SNEQV  !snow water eqv. [mm]
 ! REAL(r8), INTENT(IN)  :: h2osno_liq(-NSNOW+1:0)     ! liquid water content (col,lyr) [kg/m2]
 ! REAL(r8), INTENT(IN)  :: h2osno_ice(-NSNOW+1:0)     ! ice content (col,lyr) [kg/m2]
 ! INTEGER,  INTENT(IN)  :: snw_rds(-NSNOW+1:0)        ! snow effective radius (col,lyr) [microns, m^-6]
 ! REAL(r8), INTENT(IN)  :: mss_cnc_aer_in(-NSNOW+1:0)  ! mass concentration of all aerosol species (col,lyr,aer) [kg/kg],此处只考虑一种
 ! REAL(r8), INTENT(IN)  :: albsfc(numrad)               ! albedo of surface underlyINg snow (col,bnd) [frc]
  
  ! OUTput
  
   REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
   REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
 !-----------------------------------------------------------------------
    !
    ! variables for snow radiative transfer calculations

    ! Local variables representINg sINgle-column values of arrays:
  
    INTEGER :: snl_lcl                            ! negative number of snow layers [nbr]
    INTEGER :: snw_rds_lcl(-NSNOW+1:0)          ! snow effective radius [m^-6]
    REAL(r8):: flx_slrd_lcl(1:numrad_snw)         ! direct beam INcident irradiance [W/m2] (set to 1)
    REAL(r8):: flx_slri_lcl(1:numrad_snw)         ! diffuse INcident irradiance [W/m2] (set to 1)
    REAL(r8):: mss_cnc_aer_lcl(-NSNOW+1:0) ! aerosol mass concentration (lyr,aer_nbr) [kg/kg]
    REAL(r8):: h2osno_lcl                         ! total column snow mass [kg/m2]
    REAL(r8):: h2osno_liq_lcl(-NSNOW+1:0)       ! liquid water mass [kg/m2]
    REAL(r8):: h2osno_ice_lcl(-NSNOW+1:0)       ! ice mass [kg/m2]
    REAL(r8):: albsfc_lcl(1:numrad_snw)           ! albedo of underlyINg surface [frc]
    REAL(r8):: ss_alb_snw_lcl(-NSNOW+1:0)       ! sINgle-scatter albedo of ice graINs (lyr) [frc]
    REAL(r8):: asm_prm_snw_lcl(-NSNOW+1:0)      ! asymmetry parameter of ice graINs (lyr) [frc]
    REAL(r8):: ext_cff_mss_snw_lcl(-NSNOW+1:0)  ! mass extINction coefficient of ice graINs (lyr) [m2/kg]
    REAL(r8):: ss_alb_aer_lcl        ! sINgle-scatter albedo of aerosol species (aer_nbr) [frc] 
    REAL(r8):: asm_prm_aer_lcl       ! asymmetry parameter of aerosol species (aer_nbr) [frc]
    REAL(r8):: ext_cff_mss_aer_lcl   ! mass extINction coefficient of aerosol species (aer_nbr) [m2/kg]
    REAL(r8) :: albout(1:NBAND)               ! snow albedo, averaged INto 2 bands (=0 if no sun or no snow) (col,bnd) [frc]
    REAL(r8) :: flx_abs(-NSNOW+1:1,1:NBAND) ! absorbed flux IN each layer per unit flux INcident on top of snowpack (col,lyr,bnd) [frc]


    ! Other local variables
    INTEGER :: APRX_TYP                           ! two-stream approximation type (1=EddINgton, 2=Quadrature, 3=Hemispheric Mean) [nbr]
    INTEGER :: DELTA                              ! flag to use Delta approximation (Joseph, 1976) (1= use, 0= don't use)
    REAL(r8):: flx_wgt(1:numrad_snw)              ! weights applied to spectral bands, specific to direct and diffuse cases (bnd) [frc]
   
    INTEGER :: flg_nosnl                          ! flag: =1 if there is snow, but zero snow layers, =0 if at least 1 snow layer [flg]   
    INTEGER :: trip                               ! flag: =1 to redo RT calculation if result is unREAListic
    INTEGER :: flg_dover                          ! defINes conditions for RT redo (explaINed below)

    REAL(r8):: albedo_1                             ! temporary snow albedo [frc]
    REAL(r8):: flx_sum                            ! temporary summation variable for NIR weightINg
    REAL(r8):: albout_lcl(1:numrad_snw)             ! snow albedo by band [frc]
    REAL(r8):: flx_abs_lcl(-NSNOW+1:1,1:numrad_snw)! absorbed flux per unit INcident flux at top of snowpack (lyr,bnd) [frc]
 
    REAL(r8):: L_snw(-NSNOW+1:0)                ! h2o mass (liquid+solid) IN snow layer (lyr) [kg/m2]
    REAL(r8):: tau_snw(-NSNOW+1:0)              ! snow optical depth (lyr) [unitless]
    REAL(r8):: L_aer(-NSNOW+1:0)    ! aerosol mass IN snow layer (lyr,nbr_aer) [kg/m2] 
    REAL(r8):: tau_aer(-NSNOW+1:0)  ! aerosol optical depth (lyr,nbr_aer) [unitless]
    REAL(r8):: tau_sum                            ! cumulative (snow+aerosol) optical depth [unitless]
    REAL(r8):: tau_clm(-NSNOW+1:0)              ! column optical depth from layer bottom to snowpack top (lyr) [unitless] 
    REAL(r8):: omega_sum                          ! temporary summation of sINgle-scatter albedo of all aerosols [frc]
    REAL(r8):: g_sum                              ! temporary summation of asymmetry parameter of all aerosols [frc]

    REAL(r8):: tau(-NSNOW+1:0)                  ! weighted optical depth of snow+aerosol layer (lyr) [unitless]
    REAL(r8):: omega(-NSNOW+1:0)                ! weighted sINgle-scatter albedo of snow+aerosol layer (lyr) [frc]
    REAL(r8):: g(-NSNOW+1:0)                    ! weighted asymmetry parameter of snow+aerosol layer (lyr) [frc]
    REAL(r8):: tau_star(-NSNOW+1:0)             ! transformed (i.e. Delta-EddINgton) optical depth of snow+aerosol layer (lyr) [unitless]
    REAL(r8):: omega_star(-NSNOW+1:0)           ! transformed (i.e. Delta-EddINgton) SSA of snow+aerosol layer (lyr) [frc]
    REAL(r8):: g_star(-NSNOW+1:0)               ! transformed (i.e. Delta-EddINgton) asymmetry paramater of snow+aerosol layer (lyr) [frc]
   
    INTEGER :: g_idx, c_idx, l_idx                ! gridcell, column, and landunit INdices [idx]
    INTEGER :: bnd_idx                            ! spectral band INdex (1 <= bnd_idx <= numrad_snw) [idx]
    INTEGER :: rds_idx                            ! snow effective radius INdex for retrievINg Mie parameters from lookup table [idx]
    INTEGER :: snl_btm                            ! INdex of bottom snow layer (0) [idx]
    INTEGER :: snl_top                            ! INdex of top snow layer (-4 to 0) [idx]
    INTEGER :: fc                                 ! column filter INdex
    INTEGER :: i                                  ! layer INdex [idx]
    INTEGER :: j                                  ! aerosol number INdex [idx]
    INTEGER :: n                                  ! tridiagonal matrix INdex [idx]
    INTEGER :: m                                  ! secondary layer INdex [idx]
    INTEGER :: ix,k                               ! an INdex
   
    REAL(r8):: F_direct(-NSNOW+1:0)             ! direct-beam radiation at bottom of layer INterface (lyr) [W/m^2]
    REAL(r8):: F_net(-NSNOW+1:0)                ! net radiative flux at bottom of layer INterface (lyr) [W/m^2]
    REAL(r8):: F_abs(-NSNOW+1:0)                ! net absorbed radiative energy (lyr) [W/m^2]
    REAL(r8):: F_abs_sum                          ! total absorbed energy IN column [W/m^2]
    REAL(r8):: F_sfc_pls                          ! upward radiative flux at snowpack top [W/m^2]
    REAL(r8):: F_btm_net                          ! net flux at bottom of snowpack [W/m^2]                    
    REAL(r8):: F_sfc_net                          ! net flux at top of snowpack [W/m^2]
    REAL(r8):: energy_sum                         ! sum of all energy terms; should be 0.0 [W/m^2]
    REAL(r8):: F_direct_btm                       ! direct-beam radiation at bottom of snowpack [W/m^2]
    REAL(r8):: mu_not                             ! cosINe of solar zenith angle (used locally) [frc]

    INTEGER :: err_idx                            ! counter for number of times through error loop [nbr]
    REAL(r8):: lat_coord                          ! gridcell latitude (debuggINg only)
    REAL(r8):: lon_coord                          ! gridcell longitude (debuggINg only)
    INTEGER :: sfctype                            ! underlyINg surface type (debuggINg only)
    REAL(r8):: pi                                 ! 3.1415...
    LOGICAL                   :: opened
    INTEGER                   :: lu_unit=17
    logical, external :: wrf_dm_on_monitor



    ! INtermediate variables for radiative transfer approximation:
    REAL(r8):: gamma1(-NSNOW+1:0)               ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: gamma2(-NSNOW+1:0)               ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: gamma3(-NSNOW+1:0)               ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: gamma4(-NSNOW+1:0)               ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: lambda(-NSNOW+1:0)               ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: GAMMA(-NSNOW+1:0)                ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: mu_one                           ! two-stream coefficient from Toon et al. (lyr) [unitless]
    REAL(r8):: e1(-NSNOW+1:0)                   ! tri-diag INtermediate variable from Toon et al. (lyr) 
    REAL(r8):: e2(-NSNOW+1:0)                   ! tri-diag INtermediate variable from Toon et al. (lyr) 
    REAL(r8):: e3(-NSNOW+1:0)                   ! tri-diag INtermediate variable from Toon et al. (lyr) 
    REAL(r8):: e4(-NSNOW+1:0)                   ! tri-diag INtermediate variable from Toon et al. (lyr) 
    REAL(r8):: C_pls_btm(-NSNOW+1:0)            ! INtermediate variable: upward flux at bottom INterface (lyr) [W/m2]
    REAL(r8):: C_mns_btm(-NSNOW+1:0)            ! INtermediate variable: downward flux at bottom INterface (lyr) [W/m2]
    REAL(r8):: C_pls_top(-NSNOW+1:0)            ! INtermediate variable: upward flux at top INterface (lyr) [W/m2]
    REAL(r8):: C_mns_top(-NSNOW+1:0)            ! INtermediate variable: downward flux at top INterface (lyr) [W/m2]
    REAL(r8):: A(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: B(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: D(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: E(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: AS(-2*NSNOW+1:0)                 ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: DS(-2*NSNOW+1:0)                 ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: X(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
    REAL(r8):: Y(-2*NSNOW+1:0)                  ! tri-diag INtermediate variable from Toon et al. (2*lyr)
! ---------------------------------------------------------------------------------------------
! snicar
!ix = 0
!  do i=1, idx_Mie_snw_mx
!  do j=1, numrad_snw
!    ix = ix+1
!    ss_alb_snw_drc(i,j)        = xx_ss_alb_snw_drc(ix)
!    asm_prm_snw_drc(i,j)       = xx_asm_prm_snw_drc(ix)
!    ext_cff_mss_snw_drc(i,j)   = xx_ext_cff_mss_snw_drc(ix)
!    ss_alb_snw_dfs(i,j)        = xx_ss_alb_snw_dfs(ix)
!    asm_prm_snw_dfs(i,j)       = xx_asm_prm_snw_dfs(ix)
!    ext_cff_mss_snw_dfs(i,j)   = xx_ext_cff_mss_snw_dfs(ix)
!  end do
!  end do

!open(8,file='input.txt')
!write(8,*) 'check input albsnw:',ss_alb_snw_dfs(1,1)
!write(8,*) 'check input extsnw:', ext_cff_mss_snw_dfs(1,1)
! zero albedos for all poINts
       ! CALL snicar_init(parameters)

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.
 

    ! DefINe constants
    pi = SHR_CONST_PI

    ! always use Delta approximation for snow
    DELTA = 1
    ! Zero absorbed radiative fluxes:
       do i=-NSNOW+1,1,1
          flx_abs_lcl(:,:)   = 0._r8
          flx_abs(i,:) = 0._r8
       enddo
       h2osno_lcl = SNEQV
         

       if (h2osno_lcl > min_snw) then
        ! Assign local (single-column) variables to global values
             ! If there is snow, but zero snow layers, we must create a layer locally.
             ! This layer is presumed to have the fresh snow effective radius.
             if ( ISNOW > -1) then
                flg_nosnl         =  1
                snl_lcl           =  -1
                h2osno_ice_lcl(0) =  h2osno_lcl
                h2osno_liq_lcl(0) =  0._r8
                snw_rds_lcl(0)    =  nint(snw_rds_min)
             else
                flg_nosnl         =  0
                snl_lcl           =  ISNOW
                h2osno_liq_lcl(:) =  SNLIQ(:)
                h2osno_ice_lcl(:) =  SNICE(:)
                snw_rds_lcl(:)    =  150
             endif
          ! Set local aerosol array

             snl_btm   = 0
             snl_top   = snl_lcl+1
                       ! Set local aerosol array
             
              mss_cnc_aer_lcl(:) = BCS*1.0_r8
              

             
          ! Set spectral underlying surface albedos to their corresponding VIS or NIR albedos
          albsfc_lcl(1)                       = UALB
          albsfc_lcl(nir_bnd_bgn:nir_bnd_end) = UALB
                    ! Incident flux weighting parameters
          !  - sum of all VIS bands must equal 1
          !  - sum of all NIR bands must equal 1
          !
          ! Spectral bands (5-band case)
          !  Band 1: 0.3-0.7um (VIS)
          !  Band 2: 0.7-1.0um (NIR)
          !  Band 3: 1.0-1.2um (NIR)
          !  Band 4: 1.2-1.5um (NIR)
          !  Band 5: 1.5-5.0um (NIR)
          !  5-band weights DIRECT
                flx_wgt(1) = 1._r8
                flx_wgt(2) = 0.49352158521175_r8
                flx_wgt(3) = 0.18099494230665_r8
                flx_wgt(4) = 0.12094898498813_r8
                flx_wgt(5) = 0.20453448749347_r8
          ! Loop over snow spectral bands
          do bnd_idx = 1,numrad_snw

             mu_not    = COSZ  ! must set here, because of error handling
             flg_dover = 1              ! default is to redo
             err_idx   = 0              ! number of times through loop

             do while (flg_dover > 0)

                ! DEFAULT APPROXIMATIONS:
                !  VIS:       Delta-Eddington
                !  NIR (all): Delta-Hemispheric Mean
                !  WARNING:   DO NOT USE DELTA-EDDINGTON FOR NIR DIFFUSE - this sometimes results in negative albedo
                !  
                ! ERROR CONDITIONS:
                !  Conditions which cause "trip", resulting in redo of RT approximation:
                !   1. negative absorbed flux
                !   2. total absorbed flux greater than incident flux
                !   3. negative albedo
                !   NOTE: These errors have only been encountered in spectral bands 4 and 5
                !
                ! ERROR HANDLING
                !  1st error (flg_dover=2): switch approximation (Edd->HM or HM->Edd)
                !  2nd error (flg_dover=3): change zenith angle by 0.02 (this happens about 1 in 10^6 cases)
                !  3rd error (flg_dover=4): switch approximation with new zenith
                !  Subsequent errors: repeatedly change zenith and approximations...
              
                if ( bnd_idx == 1) then
                   if (flg_dover == 2) then
                      APRX_TYP = 3
                   elseif (flg_dover == 3) then
                      APRX_TYP = 1
                      if (COSZ > 0.5_r8) then
                         mu_not = mu_not - 0.02_r8
                      else
                         mu_not = mu_not + 0.02_r8
                      endif
                   elseif (flg_dover == 4) then
                      APRX_TYP = 3
                   else
                      APRX_TYP = 1
                   endif
                   
                else
                   if (flg_dover == 2) then
                      APRX_TYP = 1
                   elseif (flg_dover == 3) then
                      APRX_TYP = 3
                      if (COSZ > 0.5_r8) then
                         mu_not = mu_not - 0.02_r8
                      else
                         mu_not = mu_not + 0.02_r8
                      endif
                   elseif (flg_dover == 4) then
                      APRX_TYP = 1
                   else
                      APRX_TYP = 3
                   endif

                endif

                ! Set direct or diffuse incident irradiance to 1
                ! (This has to be within the bnd loop because mu_not is adjusted in rare cases)
                flx_slrd_lcl(bnd_idx) = 1._r8/(mu_not*pi) ! this corresponds to incident irradiance of 1.0
                flx_slri_lcl(bnd_idx) = 0._r8
               

                ! Pre-emptive error handling: aerosols can reap havoc on these absorptive bands.
                ! Since extremely high soot concentrations have a negligible effect on these bands, zero them.
                if ( (numrad_snw == 5).and.((bnd_idx == 5).or.(bnd_idx == 4)) ) then
                   mss_cnc_aer_lcl = 0._r8
                endif
                ! Define local Mie parameters based on snow grain size and aerosol species,
                !  retrieved from a lookup table.
                do i=snl_top,snl_btm,1
                      rds_idx = snw_rds_lcl(i) - snw_rds_min_tbl + 1
                      write(7,*) " STATS:rds_idx",rds_idx

                      ! snow optical properties (direct radiation)
                      ss_alb_snw_lcl(i)      = ss_alb_snw_drc(1,bnd_idx)
                      asm_prm_snw_lcl(i)     = asm_prm_snw_drc(1,bnd_idx)
                      ext_cff_mss_snw_lcl(i) = ext_cff_mss_snw_drc(1,bnd_idx)
            write(7,*) " STATS:ext_cff_mss_snw_lcl:",ext_cff_mss_snw_lcl(i)

                enddo
       
       ! aerosol species  optical properties only BC nocoated
                ss_alb_aer_lcl        = ss_alb_bc1(1,bnd_idx)      
                asm_prm_aer_lcl       = asm_prm_bc1(1,bnd_idx)
                ext_cff_mss_aer_lcl   = ext_cff_mss_bc1(1,bnd_idx)
                ! 1. snow and aerosol layer column mass (L_snw, L_aer [kg/m^2])
                ! 2. optical Depths (tau_snw, tau_aer)
                ! 3. weighted Mie properties (tau, omega, g)

                ! Weighted Mie parameters of each layer
                do i=snl_top,snl_btm,1
                   L_snw(i)   = h2osno_ice_lcl(i)+h2osno_liq_lcl(i)

                   tau_snw(i) = L_snw(i)*ext_cff_mss_snw_lcl(i)

            
                    L_aer(i)   = L_snw(i)*mss_cnc_aer_lcl(i)
                    tau_aer(i) = L_aer(i)*ext_cff_mss_aer_lcl
                   

                   tau_sum   = 0._r8
                   omega_sum = 0._r8
                   g_sum     = 0._r8
  
                   
                  tau_sum    = tau_sum + tau_aer(i) 
                  omega_sum  = omega_sum + (tau_aer(i)*ss_alb_aer_lcl)
                  g_sum      = g_sum + (tau_aer(i)*ss_alb_aer_lcl*asm_prm_aer_lcl)
                   

                   tau(i)    = tau_sum + tau_snw(i)
                   if(tau(i) == 0) then
                      open(unit=12,file='check.txt')
                      write(12,*) 'FATAL ERROR in SNICAR RT, tau(',i,') is the denominatoer can not equal to ',tau(i)
                      close(unit=12)
                       call endrun()
                   end if

                   omega(i)  = (1/tau(i))*(omega_sum+(ss_alb_snw_lcl(i)*tau_snw(i)))
                   g(i)      = (1/(tau(i)*omega(i)))*(g_sum+ (asm_prm_snw_lcl(i)*ss_alb_snw_lcl(i)*tau_snw(i)))
                enddo

                ! DELTA transformations, if requested
                if (DELTA == 1) then
                   do i=snl_top,snl_btm,1
                      g_star(i)     = g(i)/(1+g(i))
                      omega_star(i) = ((1-(g(i)**2))*omega(i)) / (1-(omega(i)*(g(i)**2)))
                      tau_star(i)   = (1-(omega(i)*(g(i)**2)))*tau(i)
                   enddo
                else
                   do i=snl_top,snl_btm,1
                      g_star(i)     = g(i)
                      omega_star(i) = omega(i)
                      tau_star(i)   = tau(i)
                   enddo
                endif

                ! Total column optical depth:
                ! tau_clm(i) = total optical depth above the bottom of layer i
                tau_clm(snl_top) = 0._r8
                do i=snl_top+1,snl_btm,1
                   tau_clm(i) = tau_clm(i-1)+tau_star(i-1)
                enddo

                ! Direct radiation at bottom of snowpack:
                F_direct_btm = albsfc_lcl(bnd_idx)*mu_not*exp(-(tau_clm(snl_btm)+tau_star(snl_btm))/mu_not)*pi*flx_slrd_lcl(bnd_idx)

                ! Intermediates
                ! Gamma values are approximation-specific.
                ! Eddington
                if (APRX_TYP==1) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = (7-(omega_star(i)*(4+(3*g_star(i)))))/4
                      gamma2(i) = -(1-(omega_star(i)*(4-(3*g_star(i)))))/4
                      gamma3(i) = (2-(3*g_star(i)*mu_not))/4
                      gamma4(i) = 1-gamma3(i)
                      mu_one    = 0.5
                   enddo
                   
                ! Quadrature
                elseif (APRX_TYP==2) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = (3**0.5)*(2-(omega_star(i)*(1+g_star(i))))/2
                      gamma2(i) = omega_star(i)*(3**0.5)*(1-g_star(i))/2
                      gamma3(i) = (1-((3**0.5)*g_star(i)*mu_not))/2
                      gamma4(i) = 1-gamma3(i)
                      mu_one    = 1/(3**0.5)
                   enddo

                ! Hemispheric Mean
                elseif (APRX_TYP==3) then
                   do i=snl_top,snl_btm,1
                      gamma1(i) = 2 - (omega_star(i)*(1+g_star(i)))
                      gamma2(i) = omega_star(i)*(1-g_star(i))
                      gamma3(i) = (1-((3**0.5)*g_star(i)*mu_not))/2
                      gamma4(i) = 1-gamma3(i)
                      mu_one    = 0.5
                   enddo
                endif

                ! Intermediates for tri-diagonal solution
                do i=snl_top,snl_btm,1
                   lambda(i) = sqrt(abs((gamma1(i)**2) - (gamma2(i)**2)))
                   GAMMA(i)  = gamma2(i)/(gamma1(i)+lambda(i))

                   e1(i)     = 1+(GAMMA(i)*exp(-lambda(i)*tau_star(i)))
                   e2(i)     = 1-(GAMMA(i)*exp(-lambda(i)*tau_star(i)))
                   e3(i)     = GAMMA(i) + exp(-lambda(i)*tau_star(i))
                   e4(i)     = GAMMA(i) - exp(-lambda(i)*tau_star(i))
                enddo !enddo over snow layers


                ! Intermediates for tri-diagonal solution
                do i=snl_top,snl_btm,1
                      C_pls_btm(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-(tau_clm(i)+tau_star(i))/mu_not)*   &
                                     (((gamma1(i)-(1/mu_not))*gamma3(i))+     &
                                     (gamma4(i)*gamma2(i))))/((lambda(i)**2)-(1/(mu_not**2)))

                      C_mns_btm(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-(tau_clm(i)+tau_star(i))/mu_not)*   &
                                     (((gamma1(i)+(1/mu_not))*gamma4(i))+     &
                                     (gamma2(i)*gamma3(i))))/((lambda(i)**2)-(1/(mu_not**2)))

                      C_pls_top(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-tau_clm(i)/mu_not)*(((gamma1(i)-(1/mu_not))* &
                                     gamma3(i))+(gamma4(i)*gamma2(i))))/((lambda(i)**2)-(1/(mu_not**2)))

                      C_mns_top(i) = (omega_star(i)*pi*flx_slrd_lcl(bnd_idx)* &
                                     exp(-tau_clm(i)/mu_not)*(((gamma1(i)+(1/mu_not))* &
                                     gamma4(i))+(gamma2(i)*gamma3(i))))/((lambda(i)**2)-(1/(mu_not**2)))

                 enddo

                 ! Coefficients for tridiaganol matrix solution
                do i=2*snl_lcl+1,0,1

                   !Boundary values for i=1 and i=2*snl_lcl, specifics for i=odd and i=even    
                   if (i==(2*snl_lcl+1)) then
                      A(i) = 0
                      B(i) = e1(snl_top)
                      D(i) = -e2(snl_top)
                      E(i) = flx_slri_lcl(bnd_idx)-C_mns_top(snl_top)

                   elseif(i==0) then
                      A(i) = e1(snl_btm)-(albsfc_lcl(bnd_idx)*e3(snl_btm))
                      B(i) = e2(snl_btm)-(albsfc_lcl(bnd_idx)*e4(snl_btm))
                      D(i) = 0
                      E(i) = F_direct_btm-C_pls_btm(snl_btm)+(albsfc_lcl(bnd_idx)*C_mns_btm(snl_btm))

                   elseif(mod(i,2)==-1) then   ! If odd and i>=3 (n=1 for i=3)
                      n=floor(i/2.0)
                      A(i) = (e2(n)*e3(n))-(e4(n)*e1(n))
                      B(i) = (e1(n)*e1(n+1))-(e3(n)*e3(n+1))
                      D(i) = (e3(n)*e4(n+1))-(e1(n)*e2(n+1))
                      E(i) = (e3(n)*(C_pls_top(n+1)-C_pls_btm(n)))+(e1(n)*(C_mns_btm(n)-C_mns_top(n+1)))

                   elseif(mod(i,2)==0) then    ! If even and i<=2*snl_lcl
                      n=(i/2)
                      A(i) = (e2(n+1)*e1(n))-(e3(n)*e4(n+1))
                      B(i) = (e2(n)*e2(n+1))-(e4(n)*e4(n+1))
                      D(i) = (e1(n+1)*e4(n+1))-(e2(n+1)*e3(n+1))
                      E(i) = (e2(n+1)*(C_pls_top(n+1)-C_pls_btm(n)))+(e4(n+1)*(C_mns_top(n+1)-C_mns_btm(n))) 
                   endif
                enddo

                AS(0) = A(0)/B(0)
                DS(0) = E(0)/B(0)

                do i=-1,(2*snl_lcl+1),-1
                   X(i)  = 1/(B(i)-(D(i)*AS(i+1)))
                   AS(i) = A(i)*X(i)
                   DS(i) = (E(i)-(D(i)*DS(i+1)))*X(i)
                enddo

                Y(2*snl_lcl+1) = DS(2*snl_lcl+1)
                do i=(2*snl_lcl+2),0,1
                   Y(i) = DS(i)-(AS(i)*Y(i-1))
                enddo

                ! Downward direct-beam and net flux (F_net) at the base of each layer:
                do i=snl_top,snl_btm,1
                   F_direct(i) = mu_not*pi*flx_slrd_lcl(bnd_idx)*exp(-(tau_clm(i)+tau_star(i))/mu_not)
                   F_net(i)    = (Y(2*i-1)*(e1(i)-e3(i))) + (Y(2*i)*(e2(i)-e4(i))) + &
                                 C_pls_btm(i) - C_mns_btm(i) - F_direct(i)
                enddo

                ! Upward flux at snowpack top:
                F_sfc_pls = (Y(2*snl_lcl+1)*(exp(-lambda(snl_top)*tau_star(snl_top))+ &
                            GAMMA(snl_top))) + (Y(2*snl_lcl+2)*(exp(-lambda(snl_top)* &
                            tau_star(snl_top))-GAMMA(snl_top))) + C_pls_top(snl_top)

                ! Net flux at bottom = absorbed radiation by underlying surface:
                F_btm_net = -F_net(snl_btm)


                ! Bulk column albedo and surface net flux
                albedo_1  = F_sfc_pls/((mu_not*pi*flx_slrd_lcl(bnd_idx))+flx_slri_lcl(bnd_idx))
                F_sfc_net = F_sfc_pls - ((mu_not*pi*flx_slrd_lcl(bnd_idx))+flx_slri_lcl(bnd_idx))

                trip = 0
                ! Absorbed flux in each layer
                do i=snl_top,snl_btm,1
                   if(i==snl_top) then
                      F_abs(i) = F_net(i)-F_sfc_net
                   else
                      F_abs(i) = F_net(i)-F_net(i-1)
                   endif
                   flx_abs_lcl(i,bnd_idx) = F_abs(i)
                  
          
                   ! ERROR check: negative absorption
                   if (flx_abs_lcl(i,bnd_idx) < -0.00001) then
                      trip = 1
                   endif
                enddo
                flx_abs_lcl(1,bnd_idx) = F_btm_net
             
                if (flg_nosnl == 1) then
                   ! If there are no snow layers (but still snow), all absorbed energy must be in top soil layer
                   !flx_abs_lcl(:,bnd_idx) = 0._r8
                   !flx_abs_lcl(1,bnd_idx) = F_abs(0) + F_btm_net

                   ! changed on 20070408:
                   ! OK to put absorbed energy in the fictitous snow layer because routine SurfaceRadiation
                   ! handles the case of no snow layers. Then, if a snow layer is addded between now and
                   ! SurfaceRadiation (called in Hydrology1), absorbed energy will be properly distributed.
                   flx_abs_lcl(0,bnd_idx) = F_abs(0)
                   flx_abs_lcl(1,bnd_idx) = F_btm_net
                endif
                
                !Underflow check (we've already tripped the error condition above)
                do i=snl_top,1,1
                   if (flx_abs_lcl(i,bnd_idx) < 0._r8) then
                      flx_abs_lcl(i,bnd_idx) = 0._r8
                   endif
                enddo

                F_abs_sum = 0._r8
                do i=snl_top,snl_btm,1
                   F_abs_sum = F_abs_sum + F_abs(i)
                enddo


                !ERROR check: absorption greater than incident flux
                ! (should make condition more generic than "1._r8")
                if (F_abs_sum > 1._r8) then
                   trip = 1
                endif
                !open(unit=7,file='error.txt')
                !ERROR check:
                if ((albedo_1 < 0._r8).and.(trip==0)) then
                   write(6,*) 'ERROR: albedo_1 <0 = ', albedo_1
                   trip = 1
                endif
                
                ! Set conditions for redoing RT calculation 
                if ((trip == 1).and.(flg_dover == 1)) then
                   flg_dover = 2
                elseif ((trip == 1).and.(flg_dover == 2)) then
                   flg_dover = 3
                elseif ((trip == 1).and.(flg_dover == 3)) then
                   flg_dover = 4
                elseif((trip == 1).and.(flg_dover == 4).and.(err_idx < 20)) then
                   flg_dover = 3
                   err_idx = err_idx + 1
                   write(6,*) "SNICAR WARNING: Both approximations failed with new zenith angle :(. Zenith= ", mu_not
                elseif((trip == 1).and.(flg_dover == 4).and.(err_idx >= 20)) then
                   flg_dover = 0
                   write(6,*) "SNICAR ERROR: FOUND A WORMHOLE. STUCK IN INFINITE LOOP! Called from: "
                   write(6,*) "SNICAR STATS: L_snw(0)= ", L_snw(0)
                   write(6,*) "SNICAR STATS: h2osno= ", h2osno_lcl, " snl= ", snl_lcl
                   write(6,*) "SNICAR STATS: soot1(0)= ", mss_cnc_aer_lcl
                    call endrun()
                else
                   flg_dover = 0
                endif

             enddo !enddo while (flg_dover > 0)
             ! Energy conservation check:
             ! Incident direct+diffuse radiation equals (absorbed+bulk_transmitted+bulk_reflected)
             energy_sum = (mu_not*pi*flx_slrd_lcl(bnd_idx)) + flx_slri_lcl(bnd_idx) - (F_abs_sum + F_btm_net + F_sfc_pls)
             if (abs(energy_sum) > 0.00001_r8) then
                write (6,"(a,e12.6,a,i6,a,i6)") "SNICAR ERROR: Energy conservation error of : ", energy_sum
               ! call endrun()
             endif

             albout_lcl(bnd_idx) = albedo_1


             ! Check that albedo is less than 1
             if (albout_lcl(bnd_idx) > 1.0) then

                write (6,*) "SNICAR ERROR: Albedo > 1.0 "
                write (6,*) "SNICAR STATS: bnd_idx= ",bnd_idx
                write (6,*) "SNICAR STATS: albout_lcl(bnd_idx)= ",albout_lcl(bnd_idx), " albsfc_lcl(bnd_idx)= ",albsfc_lcl(bnd_idx)
                write (6,*) "SNICAR STATS: h2osno= ", h2osno_lcl, " snl= ", snl_lcl
                write (6,*) "SNICAR STATS: coszen= ", COSZ, " flg_slr= ", 1
                write (6,*) "SNICAR STATS: soot(0)= ", mss_cnc_aer_lcl
                write (6,*) "SNICAR STATS: L_snw(-4)= ", L_snw(-4)
                write (6,*) "SNICAR STATS: L_snw(-3)= ", L_snw(-3)
                write (6,*) "SNICAR STATS: L_snw(-2)= ", L_snw(-2)
                write (6,*) "SNICAR STATS: L_snw(-1)= ", L_snw(-1)
                write (6,*) "SNICAR STATS: L_snw(0)= ", L_snw(0)
      
                albout_lcl(bnd_idx)=0.85
               ! call endrun()
             endif
          
             
          enddo   ! loop over wvl bands
      
          ! Weight output NIR albedo appropriately
          albout(1) = albout_lcl(1)
          flx_sum         = 0._r8
          do bnd_idx= nir_bnd_bgn,nir_bnd_end
             flx_sum = flx_sum + flx_wgt(bnd_idx)*albout_lcl(bnd_idx)
          end do
          albout(2) = flx_sum / sum(flx_wgt(nir_bnd_bgn:nir_bnd_end))

          ! Weight output NIR absorbed layer fluxes (flx_abs) appropriately
          flx_abs(:,1) = flx_abs_lcl(:,1)
          do i=snl_top,1,1
             flx_sum = 0._r8
             do bnd_idx= nir_bnd_bgn,nir_bnd_end
                flx_sum = flx_sum + flx_wgt(bnd_idx)*flx_abs_lcl(i,bnd_idx)
             enddo
             flx_abs(i,2) = flx_sum / sum(flx_wgt(nir_bnd_bgn:nir_bnd_end))          
          end do
      elseif ((h2osno_lcl < min_snw) .and. (h2osno_lcl > 0._r8))  then
          albout(1) = UALB
          albout(2) = UALB

       ! There is either zero snow, or no sun
      else 
          albout(1) = 0._r8
          albout(2) = 0._r8
      endif    ! if column has snow and coszen > 0
       ALBSND(1)=albout(1)
       ALBSNI(1)=albout(1)
       ALBSND(2)=albout(2)
       ALBSNI(2)=albout(2)
  END SUBROUTINE SNOWALB_SNICAR_DIR


!== begIN groundalb ================================================================================

  SUBROUTINE GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !IN
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !IN
                        TG      ,ILOC    ,JLOC    ,                   & !IN
                        ALBGRD  ,ALBGRI  )                              !OUT
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC   !grid INdex
  INTEGER,                  INTENT(IN)  :: JLOC   !grid INdex
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  REAL,                     INTENT(IN)  :: FSNO   !fraction of surface covered with snow (-)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: COSZ   !cosINe solar zenith angle (0-1)
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water content (m3/m3)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSND !direct beam snow albedo (vis, nir)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSNI !diffuse snow albedo (vis, nir)

!OUTput

  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct beam: vis, nir)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse: vis, nir)

!local 

  INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
  REAL                                  :: INC    !soil water correction factor for soil albedo
  REAL                                  :: ALBSOD !soil albedo (direct)
  REAL                                  :: ALBSOI !soil albedo (diffuse)
! --------------------------------------------------------------------------------------------------

  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.)
        IF (IST .EQ. 1)  THEN                     !soil
           ALBSOD = MIN(parameters%ALBSAT(IB)+INC,parameters%ALBDRY(IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = parameters%ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF

! INcrease desert and semi-desert albedos

!        IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
!           ALBSOD = ALBSOD + 0.10
!           ALBSOI = ALBSOI + 0.10
!        end if

        ALBGRD(IB) = ALBSOD*(1.-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.-FSNO) + ALBSNI(IB)*FSNO
  END DO

  END SUBROUTINE GROUNDALB

!== begIN twostream ================================================================================

  SUBROUTINE TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !IN
                        FWET   ,T       ,ALBGRD  ,ALBGRI  ,RHO    , & !IN
                        TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !IN
                        FAB    ,FRE     ,FTD     ,FTI     ,GDIR   , & !)   !OUT
                        FREV   ,FREG    ,BGAP    ,WGAP)

! --------------------------------------------------------------------------------------------------
! use two-stream approximation of DickINson (1983) Adv Geophysics
! 25:305-353 and Sellers (1985) Int J Remote SensINg 6:1335-1372
! to calculate fluxes absorbed by vegetation, reflected by vegetation,
! and transmitted through vegetation for unit INcomINg direct or diffuse
! flux given an underlyINg surface with known albedo.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
   INTEGER,              INTENT(IN)  :: ILOC    !grid INdex
   INTEGER,              INTENT(IN)  :: JLOC    !grid INdex
   INTEGER,              INTENT(IN)  :: IST     !surface type
   INTEGER,              INTENT(IN)  :: IB      !waveband number
   INTEGER,              INTENT(IN)  :: IC      !0=unit INcomINg direct; 1=unit INcomINg diffuse
   INTEGER,              INTENT(IN)  :: VEGTYP  !vegetation type

   REAL,                 INTENT(IN)  :: COSZ    !cosINe of direct zenith angle (0-1)
   REAL,                 INTENT(IN)  :: VAI     !one-sided leaf+stem area INdex (m2/m2)
   REAL,                 INTENT(IN)  :: FWET    !fraction of lai, sai that is wetted (-)
   REAL,                 INTENT(IN)  :: T       !surface temperature (k)

   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRD  !direct  albedo of underlyINg surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRI  !diffuse albedo of underlyINg surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: RHO     !leaf+stem reflectance
   REAL, DIMENSION(1:2), INTENT(IN)  :: TAU     !leaf+stem transmittance
   REAL,                 INTENT(IN)  :: FVEG    !green vegetation fraction [0.0-1.0]

! OUTput

   REAL, DIMENSION(1:2), INTENT(OUT) :: FAB     !flux abs by veg layer (per unit INcomINg flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FRE     !flux refl above veg layer (per unit INcomINg flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTD     !down dir flux below veg layer (per unit IN flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTI     !down dif flux below veg layer (per unit IN flux)
   REAL,                 INTENT(OUT) :: GDIR    !projected leaf+stem area IN solar direction
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREV    !flux reflected by veg layer   (per unit INcomINg flux) 
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREG    !flux reflected by ground (per unit INcomINg flux)

! local
   REAL                              :: OMEGA   !fraction of INtercepted radiation that is scattered
   REAL                              :: OMEGAL  !omega for leaves
   REAL                              :: BETAI   !upscatter parameter for diffuse radiation
   REAL                              :: BETAIL  !betai for leaves
   REAL                              :: BETAD   !upscatter parameter for direct beam radiation
   REAL                              :: BETADL  !betad for leaves
   REAL                              :: EXT     !optical depth of direct beam per unit leaf area
   REAL                              :: AVMU    !average diffuse optical depth

   REAL                              :: COSZI   !0.001 <= cosz <= 1.000
   REAL                              :: ASU     !sINgle scatterINg albedo
   REAL                              :: CHIL    ! -0.4 <= xl <= 0.6

   REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
   REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
   REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
   REAL                              :: PHI1,PHI2,SIGMA
   REAL                              :: FTDS,FTIS,FRES
   REAL                              :: DENFVEG
   REAL                              :: VAI_SPREAD
!jref:start
   REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
   REAL                              :: THETAZ
!jref:end   

!  variables for the modified two-stream scheme
!  Niu and Yang (2004), JGR

   REAL, PARAMETER :: PAI = 3.14159265 
   REAL :: HD       !crown depth (m)
   REAL :: BB       !vertical crown radius (m)
   REAL :: THETAP   !angle conversion from SZA 
   REAL :: FA       !foliage volume density (m-1)
   REAL :: NEWVAI   !effective LSAI (-)

   REAL,INTENT(INOUT) :: BGAP     !between canopy gap fraction for beam (-)
   REAL,INTENT(INOUT) :: WGAP     !withIN canopy gap fraction for beam (-)

   REAL :: KOPEN    !gap fraction for diffue light (-)
   REAL :: GAP      !total gap fraction for beam ( <=1-shafac )

! -----------------------------------------------------------------
! compute withIN and between gaps
     VAI_SPREAD = VAI
     if(VAI == 0.0) THEN
         GAP     = 1.0
         KOPEN   = 1.0
     ELSE
         IF(OPT_RAD == 1) THEN
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*parameters%RC**2)
           HD      = parameters%HVT - parameters%HVB
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/parameters%RC * TAN(ACOS(MAX(0.01,COSZ))) )
           ! BGAP    = EXP(-parameters%DEN * PAI * parameters%RC**2/COS(THETAP) )
           BGAP    = EXP(-DENFVEG * PAI * parameters%RC**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * parameters%RC**3.0 *(BB/parameters%RC)*DENFVEG)
           NEWVAI  = HD*FA
           WGAP    = (1.0-BGAP) * EXP(-0.5*NEWVAI/COSZ)
           GAP     = MIN(1.0-FVEG, BGAP+WGAP)

           KOPEN   = 0.05
         END IF

         IF(OPT_RAD == 2) THEN
           GAP     = 0.0
           KOPEN   = 0.0
         END IF

         IF(OPT_RAD == 3) THEN
           GAP     = 1.0-FVEG
           KOPEN   = 1.0-FVEG
         END IF
     end if

! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
! and OMEGA*BETAI are calculated and then divided by the new OMEGA
! because the product OMEGA*BETAI, OMEGA*BETAD is used IN solution.
! also, the transmittances and reflectances (TAU, RHO) are lINear
! weights of leaf and stem values.

     COSZI  = MAX(0.001, COSZ)
     CHIL   = MIN( MAX(parameters%XL, -0.4), 0.6)
     IF (ABS(CHIL) .LE. 0.01) CHIL = 0.01
     PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
     PHI2   = 0.877 * (1.-2.*PHI1)
     GDIR   = PHI1 + PHI2*COSZI
     EXT    = GDIR/COSZI
     AVMU   = ( 1. - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
     OMEGAL = RHO(IB) + TAU(IB)
     TMP0   = GDIR + PHI2*COSZI
     TMP1   = PHI1*COSZI
     ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
     BETADL = (1.+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
     BETAIL = 0.5 * ( RHO(IB)+TAU(IB) + (RHO(IB)-TAU(IB))   &
            * ((1.+CHIL)/2.)**2 ) / OMEGAL

! adjust omega, betad, and betai for INtercepted snow

     IF (T .GT. TFRZ) THEN                                !no snow
        TMP0 = OMEGAL
        TMP1 = BETADL
        TMP2 = BETAIL
     ELSE
        TMP0 =   (1.-FWET)*OMEGAL        + FWET*parameters%OMEGAS(IB)
        TMP1 = ( (1.-FWET)*OMEGAL*BETADL + FWET*parameters%OMEGAS(IB)*parameters%BETADS ) / TMP0
        TMP2 = ( (1.-FWET)*OMEGAL*BETAIL + FWET*parameters%OMEGAS(IB)*parameters%BETAIS ) / TMP0
     END IF

     OMEGA = TMP0
     BETAD = TMP1
     BETAI = TMP2

! absorbed, reflected, transmitted fluxes per unit INcomINg radiation

     B = 1. - OMEGA + OMEGA*BETAI
     C = OMEGA*BETAI
     TMP0 = AVMU*EXT
     D = TMP0 * OMEGA*BETAD
     F = TMP0 * OMEGA*(1.-BETAD)
     TMP1 = B*B - C*C
     H = SQRT(TMP1) / AVMU
     SIGMA = TMP0*TMP0 - TMP1
     if ( ABS (SIGMA) < 1.e-6 ) SIGMA = SIGN(1.e-6,SIGMA)
     P1 = B + AVMU*H
     P2 = B - AVMU*H
     P3 = B + TMP0
     P4 = B - TMP0
     S1 = EXP(-H*VAI)
     S2 = EXP(-EXT*VAI)
     IF (IC .EQ. 0) THEN
        U1 = B - C/ALBGRD(IB)
        U2 = B - C*ALBGRD(IB)
        U3 = F + C*ALBGRD(IB)
     ELSE
        U1 = B - C/ALBGRI(IB)
        U2 = B - C*ALBGRI(IB)
        U3 = F + C*ALBGRI(IB)
     END IF
     TMP2 = U1 - AVMU*H
     TMP3 = U1 + AVMU*H
     D1 = P1*TMP2/S1 - P2*TMP3*S1
     TMP4 = U2 + AVMU*H
     TMP5 = U2 - AVMU*H
     D2 = TMP4/S1 - TMP5*S1
     H1 = -D*P4 - C*F
     TMP6 = D - H1*P3/SIGMA
     TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
     H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
     H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
     H4 = -F*P3 - C*D
     TMP8 = H4/SIGMA
     TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
     H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
     H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
     H7 = (C*TMP2) / (D1*S1)
     H8 = (-C*TMP3*S1) / D1
     H9 = TMP4 / (D2*S1)
     H10 = (-TMP5*S1) / D2

! downward direct and diffuse fluxes below vegetation
! Niu and Yang (2004), JGR.

     IF (IC .EQ. 0) THEN
        FTDS = S2                           *(1.0-GAP) + GAP
        FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
     ELSE
        FTDS = 0.
        FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
     END IF
     FTD(IB) = FTDS
     FTI(IB) = FTIS

! flux reflected by the surface (veg. and ground)

     IF (IC .EQ. 0) THEN
        FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + ALBGRD(IB)*GAP        
        FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) 
        FREBAR = ALBGRD(IB)*GAP                   !jref - separate veg. and ground reflection
     ELSE
        FRES   = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN        
        FREVEG = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN
        FREBAR = 0                                !jref - separate veg. and ground reflection
     END IF
     FRE(IB) = FRES

     FREV(IB) = FREVEG 
     FREG(IB) = FREBAR 

! flux absorbed by vegetation

     FAB(IB) = 1. - FRE(IB) - (1.-ALBGRD(IB))*FTD(IB) &
                            - (1.-ALBGRI(IB))*FTI(IB)

!if(iloc == 1.and.jloc ==  2) then
!  write(*,'(a7,2i2,5(a6,f8.4),2(a9,f8.4))') "ib,ic: ",ib,ic," GAP: ",GAP," FTD: ",FTD(IB)," FTI: ",FTI(IB)," FRE: ", &
!         FRE(IB)," FAB: ",FAB(IB)," ALBGRD: ",ALBGRD(IB)," ALBGRI: ",ALBGRI(IB)
!end if

  END SUBROUTINE TWOSTREAM

!== begIN vege_flux ================================================================================

  SUBROUTINE VEGE_FLUX(parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !IN
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !IN
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !IN
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & !IN
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !IN
                       ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !IN
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO,          & !IN
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !IN
                       RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & !IN
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !IN
                       RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV    ,PAHG     , & !IN
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & !INOUT
                       CH      ,DX      ,DZ8W    ,                   & !
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !OUT
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & !OUT
                       T2MV    ,PSNSUN  ,PSNSHA  ,                   & !OUT
                       QC      ,QSFC    ,PSFC    ,                   & !IN
                       Q2V     ,CAH2    ,CHLEAF  ,CHUC,              & !INOUT 
                       SH2O,JULIAN, SWDOWN, PRCP, FB, FSR, GECROS1D)      ! Gecros 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve for vegetation (tv) and
! ground (tg) temperatures that balance the surface energy budgets

! vegetated:
! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC   !grid INdex
  INTEGER,                         INTENT(IN) :: JLOC   !grid INdex
  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
  REAL,                            INTENT(INOUT) :: SAV    !solar rad absorbed by veg (w/m2)
  REAL,                            INTENT(INOUT) :: SAG    !solar rad absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wINd speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wINd speed IN eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wINd speed IN northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temp at reference height (k)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at zlvl (pa)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at zlvl (kg/kg)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m**3)
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
  REAL,                            INTENT(IN) :: CWP    !canopy wINd parameter

  REAL,                            INTENT(IN) :: VAI    !total leaf area INdex + stem area INdex
  REAL,                            INTENT(IN) :: LAISUN !sunlit leaf area INdex, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: LAISHA !shaded leaf area INdex, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum (m)
  REAL,                            INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMV    !vegetation emissivity
  REAL,                            INTENT(IN) :: EMG    !ground emissivity

  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thINkness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: CANLIQ !INtercepted liquid water (mm)
  REAL,                            INTENT(IN) :: CANICE !INtercepted ice mass (mm)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAV  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAV !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAG  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAG !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
  REAL,                            INTENT(IN) :: IGS    !growINg season INdex (0=off, 1=on)
  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity IN surface soil/snow air space (-)

  REAL                           , INTENT(IN) :: QC     !cloud water mixINg ratio
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !grid spacINg
  REAL                           , INTENT(IN) :: Q2     !mixINg ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(INOUT) :: QSFC   !mixINg ratio at lowest model layer
  REAL, INTENT(IN)   :: PAHV  !precipitation advected heat - canopy net IN (W/m2)
  REAL, INTENT(IN)   :: PAHG  !precipitation advected heat - ground net IN (W/m2)

! INput/OUTput
  REAL,                         INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL,                         INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL,                         INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL,                         INTENT(INOUT) :: TG     !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! OUTput
! -FSA + FIRA + FSH + (FCEV + FCTR + FGEV) + FCST + SSOIL = 0
  REAL,                           INTENT(OUT) :: TAUXV  !wINd stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYV  !wINd stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRC    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHC    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: IRG    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHG    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: TR     !transpiration heat flux (w/m2)[+= to atm]
  REAL,                           INTENT(OUT) :: GH     !ground heat (w/m2) [+ = to soil]
  REAL,                           INTENT(OUT) :: T2MV   !2 m height air temperature (k)
  REAL,                           INTENT(OUT) :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: CHLEAF !leaf exchange coefficient
  REAL,                           INTENT(OUT) :: CHUC   !under canopy exchange coefficient

  REAL,                           INTENT(OUT) :: Q2V
  REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL :: U10V    !10 m wINd speed IN eastward dir (m/s) 
  REAL :: V10V    !10 m wINd speed IN eastward dir (m/s) 
  REAL :: WSPD

! ------------------------ local variables ----------------------------------------------------
  REAL :: CW           !water vapor exchange coefficient
  REAL :: FV           !friction velocity (m/s)
  REAL :: WSTAR        !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H          !roughness length, sensible heat (m)
  REAL :: Z0HG         !roughness length, sensible heat (m)
  REAL :: RB           !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMC         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHC         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC         !aerodynamic resistance for water vapor (s/m)
  REAL :: RAMG         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHG         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWG         !aerodynamic resistance for water vapor (s/m)

  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)

  REAL :: MOL          !MonIN-Obukhov length (m)
  REAL :: DTV          !change IN tv, last iteration (k)
  REAL :: DTG          !change IN tg, last iteration (k)

  REAL :: AIR,CIR      !coefficients for ir as function of ts**4
  REAL :: CSH          !coefficients for sh as function of ts
  REAL :: CEV          !coefficients for ev as function of esat[ts]
  REAL :: CGH          !coefficients for st as function of ts
  REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
  REAL :: ATA,BTA      !coefficients for tah as function of ts
  REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]

  REAL :: ESTV         !saturation vapor pressure at tv (pa)
  REAL :: ESTG         !saturation vapor pressure at tg (pa)
  REAL :: DESTV        !d(es)/dt at ts (pa/k)
  REAL :: DESTG        !d(es)/dt at tg (pa/k)
  REAL :: ESATW        !es for water
  REAL :: ESATI        !es for ice
  REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
  REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice

  REAL :: FM           !momentum stability correction, weighted by prior iters
  REAL :: FH           !sen heat stability correction, weighted by prior iters
  REAL :: FHG          !sen heat stability correction, ground
  REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]

  REAL :: A            !temporary calculation
  REAL :: B            !temporary calculation
  REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
  REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
  REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
  REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
  REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
  REAL :: COND         !sum of conductances (s/m)
  REAL :: UC           !wINd speed at top of canopy (m/s)
  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: H            !temporary sensible heat flux (w/m2)
  REAL :: HG           !temporary sensible heat flux (w/m2)
  REAL :: MOZ          !MonIN-Obukhov stability parameter
  REAL :: MOZG         !MonIN-Obukhov stability parameter
  REAL :: MOZOLD       !MonIN-Obukhov stability parameter from prior iteration
  REAL :: FM2          !MonIN-Obukhov momentum adjustment at 2m
  REAL :: FH2          !MonIN-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m
  REAL :: THSTAR          !Surface exchange at 2m

  REAL :: THVAIR
  REAL :: THAH 
  REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
  REAL, INTENT(OUT):: CAH2         !sensible heat conductance for diagnostics
  REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
  REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
  REAL :: EAH2         !2m vapor pressure over canopy
  REAL :: QFX        !moisture flux
  REAL :: E1           


  REAL :: VAIE         !total leaf area INdex + stem area INdex,effective
  REAL :: LAISUNE      !sunlit leaf area INdex, one-sided (m2/m2),effective
  REAL :: LAISHAE      !shaded leaf area INdex, one-sided (m2/m2),effective

  INTEGER :: K         !INdex
  INTEGER :: ITER      !iteration INdex

!jref - NITERC test from 5 to 20  
  INTEGER, PARAMETER :: NITERC = 20   !number of iterations for surface temperature
!jref - NITERG test from 3-5
  INTEGER, PARAMETER :: NITERG = 5   !number of iterations for ground temperature
  INTEGER :: MOZSGN    !number of times MOZ changes sign
  REAL    :: MPE       !prevents overflow error if division by zero

  INTEGER :: LITER     !Last iteration

  REAL, INTENT(IN)                    :: JULIAN, SWDOWN, PRCP, FB
  REAL, INTENT(INOUT)                 :: FSR
  REAL,DIMENSION(1:60), INTENT(INOUT) :: GECROS1D 
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O    !soil liquid water
  
  REAL :: ROOTD, WUL, WLL, Thickness, TLAIE, GLAIE, TLAI, GLAI, FRSU
  INTEGER :: NROOT, J


  REAL :: T, TDC       !KelvIN to degree Celsius with limit -50 to +50

  character(len=80) ::  message

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )
! ---------------------------------------------------------------------------------------------

        MPE = 1E-6
        LITER = 0
        FV = 0.1

! ---------------------------------------------------------------------------------------------
! INitialization variables that do not depend on stability iteration
! ---------------------------------------------------------------------------------------------
        DTV = 0.
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        HG     = 0.
        H      = 0.
        QFX    = 0.

! limit LAI

        VAIE    = MIN(6.,VAI   )
        LAISUNE = MIN(6.,LAISUN)
        LAISHAE = MIN(6.,LAISHA)

! saturation vapor pressure at ground temperature

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
           ESTG = ESATW
        ELSE
           ESTG = ESATI
        END IF

!jref - consistent surface specific humidity for sfcdif3 and sfcdif4

        QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  

! canopy height

        HCAN = parameters%HVT
        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
        UC = UR*LOG((HCAN-ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7
        IF((HCAN-ZPD) <= 0.) THEN
          WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
          call wrf_message ( message )
          WRITE(message,*) 'i,j poINt=',ILOC, JLOC
          call wrf_message ( message )
          WRITE(message,*) 'HCAN  =',HCAN
          call wrf_message ( message )
          WRITE(message,*) 'ZPD   =',ZPD
          call wrf_message ( message )
          write (message, *) 'SNOWH =',SNOWH
          call wrf_message ( message )
          call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
        END IF

! prepare for longwave rad.

        AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*SB*TG**4  
        CIR = (2.-EMV*(1.-EMG))*EMV*SB
! ---------------------------------------------------------------------------------------------
      loop1: DO ITER = 1, NITERC    !  begIN stability iteration

       IF(ITER == 1) THEN
            Z0H  = Z0M  
            Z0HG = Z0MG
       ELSE
            Z0H  = Z0M    !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
            Z0HG = Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
       END IF

! aerodyn resistances between heights zlvl and d+z0v

       IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !IN
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !IN
                       MPE    ,ILOC   ,JLOC   ,                 & !IN
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !INOUT
                       CM     ,CH     ,FV     ,CH2     )          !OUT
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & !IN
                       ZLVL   ,ILOC   ,JLOC   ,         & !IN
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !IN
                       FV     )                                   !OUT
          ! Undo the multiplication by wINdspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
       ENDIF

       RAMC = MAX(1.,1./(CM*UR))
       RAHC = MAX(1.,1./(CH*UR))
       RAWC = RAHC

! aerodyn resistance between heights z0g and d+z0v, RAG, and leaf
! boundary layer resistance, RB
       
       CALL RAGRB(parameters,ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !IN
                  ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !IN
                  Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !IN
                  TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !INOUT
                  RAMG   ,RAHG   ,RAWG   ,RB     )           !OUT

! es and d(es)/dt evaluated at tv

       T = TDC(TV)
       CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
       IF (T .GT. 0.) THEN
          ESTV  = ESATW
          DESTV = DSATW
       ELSE
          ESTV  = ESATI
          DESTV = DSATI
       END IF

! stomatal resistance
        
     IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  ! Ball-Berry
         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !IN       
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !IN
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !IN
                       RSSUN ,PSNSUN)                         !OUT

         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !IN
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !IN
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !IN
                       RSSHA ,PSNSHA)                         !OUT
        END IF

        IF (OPT_CRS == 2) then  ! Jarvis
         CALL  CANRES (parameters,PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !IN
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          !OUT

         CALL  CANRES (parameters,PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !IN
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          !OUT
        END IF

     ! Call Gecros
       IF (opt_crop == 2) then
         IF ((GECROS1D(41).GT.0).and.(GECROS1D(42).LT.0.)) then   !Gecros
           Thickness = 0.
           NROOT = 0
           ROOTD = GECROS1D(33)
           WUL = 0.
           WLL = 0.

           DO J = 1,NSOIL
            Thickness = Thickness + DZSNSO (J)
            if (Thickness.lt.ROOTD/100.) then
              NROOT = NROOT + 1
            endif
           ENDDO
          
           NROOT = NROOT + 1
           NROOT = MAX(1,NROOT)
        
           Thickness = 0.
       
           DO J = 1,NROOT
             Thickness = Thickness + DZSNSO (J) 
             if (Thickness.gt.ROOTD/100.) then
                WUL = WUL + ((ROOTD/100.-Thickness+DZSNSO(J))*1000.*(SH2O(J)-parameters%SMCWLT(J)))
             else
                WUL = WUL + (DZSNSO(J)*1000.*(SH2O(J)-parameters%SMCWLT(J)))
             endif
           ENDDO
       
           DO J = 1,NSOIL
             WLL = WLL + (DZSNSO(J)*1000.*(SH2O(J)-parameters%SMCWLT(J)))
           ENDDO
           WLL = WLL - WUL
         
           CALL gecros (JULIAN, DT, 1, RB, RAHC, RAHG+RSURF, FB, SNOWH        , & !I
               UR, SFCTMP, EAIR, SWDOWN, LWDN, PRCP, WUL, WLL                      , & !I
               parameters%SMCWLT(1), parameters%DLEAF                              , & !I
               GECROS1D                                                        , & !H                                    
               SAV, SAG, FSR, FRSU, RSSUN, RSSHA)                    !O
           
               GLAI = GECROS1D(49)
               TLAI = GECROS1D(50)

               ! effective LAIs
               TLAIE    = MIN(6.,TLAI    / FVEG)
               GLAIE    = MIN(6.,GLAI    / FVEG) 
         ENDIF     
       ENDIF     
       
     END IF

! prepare for sensible heat flux above veg.

        CAH  = 1./RAHC
        CVH  = 2.*VAIE/RB
        CGH  = 1./RAHG
        COND = CAH + CVH + CGH
        ATA  = (SFCTMP*CAH + TG*CGH) / COND
        BTA  = CVH/COND
        CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH

! prepare for latent heat flux above veg.

        CAW  = 1./RAWC
        CEW  = FWET*VAIE/RB

        IF (OPT_CROP /= 2) THEN
          CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        ELSE
           !RSSUN and RSSHA are IN resistance per unit LAI IN the Jarvis and Ball-Berry!. RSSUN and RSSHA of Gecros are IN s/m
          CTW  = (1.-FWET)*(1./(RB/(FRSU*GLAIE)+RSSUN) + 1./(RB/((1.-FRSU)*GLAIE)+RSSHA)) !transpiration conductance leaf to canopy air
        ENDIF
        CGW  = 1./(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   ! Barlage: change to vegetation v3.6
        CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMAV

! evaluate surface fluxes with current temperature and solve for dts

        TAH = ATA + BTA*TV               ! canopy air T.
        EAH = AEA + BEA*ESTV             ! canopy air e

        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV ! Barlage: change to v IN v3.6
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV
	IF (TV > TFRZ) THEN
          EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    ! Barlage: add if block for canice IN v3.6
	ELSE
          EVC = MIN(CANICE*LATHEAV/DT,EVC)
	END IF

        B   = SAV-IRC-SHC-EVC-TR+PAHV                          !additional w/m2
        A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) !volumetric heat capacity
        DTV = B/A

        IRC = IRC + FVEG*4.*CIR*TV**3*DTV
        SHC = SHC + FVEG*CSH*DTV
        EVC = EVC + FVEG*CEV*DESTV*DTV
        TR  = TR  + FVEG*CTR*DESTV*DTV                               

! update vegetation surface temperature
        TV  = TV + DTV
!        TAH = ATA + BTA*TV               ! canopy air T; update here for consistency

! for computINg M-O length IN the next iteration
        H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
        HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG

! consistent specific humidity from canopy air vapor pressure
        QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)

        IF (LITER == 1) THEN
           exit loop1 
        ENDIF
        IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
           LITER = 1
        ENDIF

     END DO loop1 ! end stability iteration

! under-canopy fluxes and tg

        AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        CIR = EMG*SB
        CSH = RHOAIR*CPAIR/RAHG
        CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  ! Barlage: change to ground v3.6
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

     loop2: DO ITER = 1, NITERG

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        IRG = CIR*TG**4 + AIR
        SHG = CSH * (TG         - TAH         )
        EVG = CEV * (ESTG*RHSUR - EAH         )
        GH  = CGH * (TG         - STC(ISNOW+1))

        B = SAG-IRG-SHG-EVG-GH+PAHG
        A = 4.*CIR*TG**3+CSH+CEV*DESTG+CGH
        DTG = B/A

        IRG = IRG + 4.*CIR*TG**3*DTG
        SHG = SHG + CSH*DTG
        EVG = EVG + CEV*DESTG*DTG
        GH  = GH  + CGH*DTG
        TG  = TG  + DTG

     END DO loop2
     
!     TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        IF(OPT_STC == 1) TG  = TFRZ
        IF(OPT_STC == 3) TG  = (1.-FSNO)*TG + FSNO*TFRZ   ! MB: allow TG>0C durINg melt v3.7
        IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG+PAHG - (IRG+SHG+EVG)
     END IF
     END IF

! wINd stresses

     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV

! consistent vegetation air temperature and vapor pressure sINce TG is not consistent with the TAH/EAH
! calculation.
!     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
!     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
!     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
!     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG

! 2m temperature over vegetation ( corrected for low CQ2V values )
   IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN
!      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.E-5 ) THEN
         T2MV = TAH
!         Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         Q2V  = QSFC
      ELSE
         T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1./CAH2
!         Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
   ENDIF

! update CH for OUTput
     CH = CAH
     CHLEAF = CVH
     CHUC = 1./RAHG

  END SUBROUTINE VEGE_FLUX

!== begIN bare_flux ================================================================================

  SUBROUTINE BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !IN
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !IN
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !IN
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,FSNO    , & !IN
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !IN
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !IN
                        TGB     ,CM      ,CH      ,          & !INOUT
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !OUT
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !OUT
                        QC      ,QSFC    ,PSFC    ,                   & !IN
                        SFCPRS  ,Q2B     ,EHB2    )                     !IN 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for bare soil fraction.

! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN) :: ILOC   !grid INdex
  INTEGER                        , INTENT(IN) :: JLOC   !grid INdex
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wINd speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wINd speed IN eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wINd speed IN northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temperature at height zlvl (k)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity IN surface soil/snow air space (-)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

!jref:start; IN 
  INTEGER                        , INTENT(IN) :: IVGTYP
  REAL                           , INTENT(IN) :: QC     !cloud water mixINg ratio
  REAL                           , INTENT(INOUT) :: QSFC   !mixINg ratio at lowest model layer
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !horisontal grid spacINg
  REAL                           , INTENT(IN) :: Q2     !mixINg ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!jref:end
  REAL, INTENT(IN)   :: PAHB  !precipitation advected heat - ground net IN (W/m2)

! INput/OUTput
  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! OUTput
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0

  REAL,                           INTENT(OUT) :: TAUXB  !wINd stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYB  !wINd stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
!jref:start
  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
  REAL :: EHB    !bare ground heat conductance
  REAL :: U10B    !10 m wINd speed IN eastward dir (m/s)
  REAL :: V10B    !10 m wINd speed IN eastward dir (m/s)
  REAL :: WSPD
!jref:end

! local variables 

  REAL :: TAUX       !wINd stress: e-w (n/m2)
  REAL :: TAUY       !wINd stress: n-s (n/m2)
  REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
  REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
  REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
  REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
  REAL :: FIRE       !emitted ir (w/m2)
  REAL :: TRAD       !radiative temperature (k)
  REAL :: TAH        !"surface" temperature at height z0h+zpd (k)

  REAL :: CW         !water vapor exchange coefficient
  REAL :: FV         !friction velocity (m/s)
  REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H        !roughness length, sensible heat, ground (m)
  REAL :: RB         !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
  REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
  REAL :: MOL        !MonIN-Obukhov length (m)
  REAL :: DTG        !change IN tg, last iteration (k)

  REAL :: CIR        !coefficients for ir as function of ts**4
  REAL :: CSH        !coefficients for sh as function of ts
  REAL :: CEV        !coefficients for ev as function of esat[ts]
  REAL :: CGH        !coefficients for st as function of ts

!jref:start
  REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
  REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
  REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics
  REAL :: CH2B       !exchange coefficient for 2m temp.
  REAL :: CQ2B       !exchange coefficient for 2m temp.
  REAL :: THVAIR     !virtual potential air temp
  REAL :: THGH       !potential ground temp
  REAL :: EMB        !momentum conductance
  REAL :: QFX        !moisture flux
  REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
  INTEGER :: VEGTYP     !vegetation type set to isbarren
  REAL :: E1
!jref:end

  REAL :: ESTG       !saturation vapor pressure at tg (pa)
  REAL :: DESTG      !d(es)/dt at tg (pa/K)
  REAL :: ESATW      !es for water
  REAL :: ESATI      !es for ice
  REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
  REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice

  REAL :: A          !temporary calculation
  REAL :: B          !temporary calculation
  REAL :: H          !temporary sensible heat flux (w/m2)
  REAL :: MOZ        !MonIN-Obukhov stability parameter
  REAL :: MOZOLD     !MonIN-Obukhov stability parameter from prior iteration
  REAL :: FM         !momentum stability correction, weighted by prior iters
  REAL :: FH         !sen heat stability correction, weighted by prior iters
  INTEGER :: MOZSGN  !number of times MOZ changes sign
  REAL :: FM2          !MonIN-Obukhov momentum adjustment at 2m
  REAL :: FH2          !MonIN-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m

  INTEGER :: ITER    !iteration INdex
  INTEGER :: NITERB  !number of iterations for surface temperature
  REAL    :: MPE     !prevents overflow error if division by zero
!jref:start
!  DATA NITERB /3/
  DATA NITERB /5/
  SAVE NITERB
  REAL :: T, TDC     !KelvIN to degree Celsius with limit -50 to +50
  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

! -----------------------------------------------------------------
! INitialization variables that do not depend on stability iteration
! -----------------------------------------------------------------
        MPE = 1E-6
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        H      = 0.
        QFX    = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

! -----------------------------------------------------------------
      loop3: DO ITER = 1, NITERB  ! begIN stability iteration

        IF(ITER == 1) THEN
            Z0H = Z0M 
        ELSE
            Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        END IF

        IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !IN
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !IN
                       MPE    ,ILOC   ,JLOC   ,                 & !IN
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !INOUT
                       CM     ,CH     ,FV     ,CH2     )          !OUT
        ENDIF

        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !IN
                       ZLVL   ,ILOC   ,JLOC   ,         & !IN
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !IN
                       FV     )                                   !OUT
          ! Undo the multiplication by wINdspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
          IF(SNOWH > 0.) THEN
             CM = MIN(0.01,CM)   ! CM & CH are too large, causINg
             CH = MIN(0.01,CH)   ! computational INstability
          END IF

        ENDIF

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB

!jref - variables for diagnostics         
        EMB = 1./RAMB
        EHB = 1./RAHB

! es and d(es)/dt evaluated at tg

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        CSH = RHOAIR*CPAIR/RAHB
        CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)

! surface fluxes and dtg

        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))

        B     = SAG-IRB-SHB-EVB-GHB+PAHB
        A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A

        IRB = IRB + 4.*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG

! update ground surface temperature
        TGB = TGB + DTG

! for M-O length
        H = CSH * (TGB - SFCTMP)

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))

        QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR

     END DO loop3 ! end stability iteration
! -----------------------------------------------------------------

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          IF(OPT_STC == 1) TGB = TFRZ
          IF(OPT_STC == 3) TGB  = (1.-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C durINg melt v3.7
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG+PAHB - (IRB+SHB+EVB)
     END IF
     END IF

! wINd stresses
         
     TAUXB = -RHOAIR*CM*UR*UU
     TAUYB = -RHOAIR*CM*UR*VV

!jref:start; errors IN origINal equation corrected.
! 2m air temperature
     IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
       EHB2  = FV*VKC/LOG((2.+Z0H)/Z0H)
       EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
       CQ2B  = EHB2
       IF (EHB2.lt.1.E-5 ) THEN
         T2MB  = TGB
         Q2B   = QSFC
       ELSE
         T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
         Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
       ENDIF
       IF (parameters%urban_flag) Q2B = QSFC
     END IF

! update CH 
     CH = EHB

  END SUBROUTINE BARE_FLUX

!== begIN ragrb ====================================================================================

  SUBROUTINE RAGRB(parameters,ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & !IN
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !IN
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !IN
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !INOUT
                   RAMG   ,RAHG   ,RAWG   ,RB     )           !OUT
! --------------------------------------------------------------------------------------------------
! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
! resistance RB
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,              INTENT(IN) :: ILOC   !grid INdex
  INTEGER,              INTENT(IN) :: JLOC   !grid INdex
  INTEGER,              INTENT(IN) :: ITER   !iteration INdex
  INTEGER,              INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                 INTENT(IN) :: VAI    !total LAI + stem area INdex, one sided
  REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                 INTENT(IN) :: HG     !ground sensible heat flux (w/m2)
  REAL,                 INTENT(IN) :: TV     !vegetation temperature (k)
  REAL,                 INTENT(IN) :: TAH    !air temperature at height z0h+zpd (k)
  REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                 INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                 INTENT(IN) :: HCAN   !canopy height (m) [note: hcan >= z0mg]
  REAL,                 INTENT(IN) :: UC     !wINd speed at top of canopy (m/s)
  REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat (m)
  REAL,                 INTENT(IN) :: Z0HG   !roughness length, sensible heat, ground (m)
  REAL,                 INTENT(IN) :: FV     !friction velocity (m/s)
  REAL,                 INTENT(IN) :: CWP    !canopy wINd parameter
  REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero

! IN & OUT

  REAL,              INTENT(INOUT) :: MOZG   !MonIN-Obukhov stability parameter
  REAL,              INTENT(INOUT) :: FHG    !stability correction

! OUTputs
  REAL                             :: RAMG   !aerodynamic resistance for momentum (s/m)
  REAL                             :: RAHG   !aerodynamic resistance for sensible heat (s/m)
  REAL                             :: RAWG   !aerodynamic resistance for water vapor (s/m)
  REAL                             :: RB     !bulk leaf boundary layer resistance (s/m)


  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: TMP1         !temporary calculation
  REAL :: TMP2         !temporary calculation
  REAL :: TMPRAH2      !temporary calculation for aerodynamic resistances
  REAL :: TMPRB        !temporary calculation for rb
  REAL :: MOLG,FHGNEW,CWPC
! --------------------------------------------------------------------------------------------------
! stability correction to below canopy resistance

       MOZG = 0.
       MOLG = 0.

       IF(ITER > 1) THEN
        TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
        IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
        MOLG = -1. * FV**3 / TMP1
        MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
       END IF

       IF (MOZG < 0.) THEN
          FHGNEW  = (1. - 15.*MOZG)**(-0.25)
       ELSE
          FHGNEW  = 1.+ 4.7*MOZG
       ENDIF

       IF (ITER == 1) THEN
          FHG = FHGNEW
       ELSE
          FHG = 0.5 * (FHG+FHGNEW)
       ENDIF

       CWPC = (CWP * VAI * HCAN * FHG)**0.5
!       CWPC = (CWP*FHG)**0.5

       TMP1 = EXP( -CWPC*Z0HG/HCAN )
       TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
       TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)

! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.

       KH  = MAX ( VKC*FV*(HCAN-ZPD), MPE )
       RAMG = 0.
       RAHG = TMPRAH2 / KH
       RAWG = RAHG

! leaf boundary layer resistance

       TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
       RB     = TMPRB * SQRT(parameters%DLEAF/UC)
       RB     = MAX(RB,100.0)
!       RB = 200

  END SUBROUTINE RAGRB

!== begIN sfcdif1 ==================================================================================

  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !IN
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !IN
       &             MPE    ,ILOC   ,JLOC   ,                 & !IN
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !INOUT
       &             CM     ,CH     ,FV     ,CH2     )          !OUT
! -------------------------------------------------------------------------------------------------
! computINg surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! INputs
    
  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,              INTENT(IN) :: ILOC   !grid INdex
    INTEGER,              INTENT(IN) :: JLOC   !grid INdex
    INTEGER,              INTENT(IN) :: ITER   !iteration INdex
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wINd speed (m/s)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! IN & OUT

    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !MonIN-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters

! OUTputs

    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

! locals
    REAL    :: MOL                      !MonIN-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !MonIN-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
! -------------------------------------------------------------------------------------------------
! MonIN-Obukhov stability parameter moz for next iteration

    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal("STOP IN Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF

! accumulate number of times moz changes sign.

    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF

! evaluate stability-dependent variables usINg moz from prior iteration
    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2

! 2-meter
       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF

! except for first iteration, weight stability factors for previous
! iteration to help avoid flip-flops from one iteration to the next

    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF

! exchange coefficients

    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
! friction velocity

    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1

!== begIN sfcdif2 ==================================================================================

  SUBROUTINE SFCDIF2(parameters,ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !IN
                     ZLM    ,ILOC   ,JLOC   ,         & !IN
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !IN
                     USTAR  )                                   !OUT

! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, INTENT(INOUT) :: AKMS
    REAL, INTENT(INOUT) :: AKHS
    REAL, INTENT(INOUT) :: RLMO
    REAL, INTENT(INOUT) :: WSTAR2
    REAL,   INTENT(OUT) :: USTAR

    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4

    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA

    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
! PAULSON'S SURFACE FUNCTIONS
    PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
         &        +2.* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.* YY
    PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)
    PSPHS (YY)= 5.* YY

! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN ZilitINkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
    ILECH = 0

! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0

! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)

! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
    ELSE
       IF (RLMO .lt. 0.)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
       END IF

! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ZILITINKEVITCH FIX FOR ZT
       ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       IF(SIMM < 1.e-6) SIMM = 1.e-6        ! Limit stability function
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
       IF(SIMH < 1.e-6) SIMH = 1.e-6        ! Limit stability function
       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
       ELSE
          WSTAR2 = 0.0
       END IF
!-----------------------------------------------------------------------
       RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
       RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
       RLMO = RLMA

!       write(*,'(a20,10f15.6)')'SFCDIF: RLMO=',RLMO,RLMN,ELFC , AKHS , DTHV , USTAR
!    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF2

!== begIN esat =====================================================================================

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
!---------------------------------------------------------------------------------------------------
! use polynomials to calculate saturation vapor pressure and derivative with
! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! IN

  REAL, INTENT(IN)  :: T              !temperature

!OUT

  REAL, INTENT(OUT) :: ESW            !saturation vapor pressure over water (pa)
  REAL, INTENT(OUT) :: ESI            !saturation vapor pressure over ice (pa)
  REAL, INTENT(OUT) :: DESW           !d(esat)/dt over water (pa/K)
  REAL, INTENT(OUT) :: DESI           !d(esat)/dt over ice (pa/K)

! local

  REAL :: A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
  REAL :: B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
  REAL :: C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
  REAL :: D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT

!== begIN stomata ==================================================================================

  SUBROUTINE STOMATA (parameters,VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & !IN
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & !IN
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & !IN
                      RS      ,PSN     )                              !OUT
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
      INTEGER,INTENT(IN)  :: ILOC   !grid INdex
      INTEGER,INTENT(IN)  :: JLOC   !grid INdex
      INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type

      REAL, INTENT(IN)    :: IGS    !growINg season INdex (0=off, 1=on)
      REAL, INTENT(IN)    :: MPE    !prevents division by zero errors

      REAL, INTENT(IN)    :: TV     !foliage temperature (k)
      REAL, INTENT(IN)    :: EI     !vapor pressure INside leaf (sat vapor press at tv) (pa)
      REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
      REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
      REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa)
      REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
      REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
      REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
      REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
      REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
      REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)

! OUTput
      REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
      REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]

! IN&OUT
      REAL                :: RLB    !boundary layer resistance (s m2 / umol)
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
      INTEGER :: ITER     !iteration INdex
      INTEGER :: NITER    !number of iterations

      DATA NITER /3/
      SAVE NITER

      REAL :: AB          !used IN statement functions
      REAL :: BC          !used IN statement functions
      REAL :: F1          !generic temperature response (statement function)
      REAL :: F2          !generic temperature INhibition (statement function)
      REAL :: TC          !foliage temperature (degree Celsius)
      REAL :: CS          !co2 concentration at leaf surface (pa)
      REAL :: KC          !co2 Michaelis-Menten constant (pa)
      REAL :: KO          !o2 Michaelis-Menten constant (pa)
      REAL :: A,B,C,Q     !INtermediate calculations for RS
      REAL :: R1,R2       !roots for RS
      REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
      REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
      REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
      REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
      REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
      REAL :: CP          !co2 compensation poINt (pa)
      REAL :: CI          !INternal co2 (pa)
      REAL :: AWC         !INtermediate calculation for wc
      REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
      REAL :: J           !electron transport (umol co2/m2/s)
      REAL :: CEA         !constraIN ea or else model blows up
      REAL :: CF          !s m2/umol -> s/m

      F1(AB,BC) = AB**((BC-25.)/10.)
      F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
      REAL :: T
! ---------------------------------------------------------------------------------------------

! INitialize RS=RSMAX and PSN=0 because will only do calculations
! for APAR > 0, IN which case RS <= RSMAX and PSN >= 0

         CF = SFCPRS/(8.314*SFCTMP)*1.e06
         RS = 1./parameters%BP * CF
         PSN = 0.

         IF (APAR .LE. 0.) RETURN

         FNF = MIN( FOLN/MAX(MPE,parameters%FOLNMX), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*parameters%QE25
         KC  = parameters%KC25 * F1(parameters%AKC,TC)
         KO  = parameters%KO25 * F1(parameters%AKO,TC)
         AWC = KC * (1.+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21
         VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX,TC)

! first guess ci

         CI = 0.7*CO2*parameters%C3PSN + 0.4*CO2*(1.-parameters%C3PSN)

! rb: s/m -> s m**2 / umol

         RLB = RB/CF

! constraIN ea

         CEA = MAX(0.25*EI*parameters%C3PSN+0.40*EI*(1.-parameters%C3PSN), MIN(EA,EI) )

! ci iteration
!jref: C3PSN is equal to 1 for all veg types.
       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*parameters%C3PSN  + J*(1.-parameters%C3PSN)
            WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*parameters%C3PSN + VCMX*(1.-parameters%C3PSN)
            WE = 0.5*VCMX*parameters%C3PSN + 4000.*VCMX*CI/SFCPRS*(1.-parameters%C3PSN)
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
            B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.
            C = -RLB
            IF (B .GE. 0.) THEN
               Q = -0.5*( B + SQRT(B*B-4.*A*C) )
            ELSE
               Q = -0.5*( B - SQRT(B*B-4.*A*C) )
            END IF
            R1 = Q/A
            R2 = C/Q
            RS = MAX(R1,R2)
            CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
       END DO 

! rs, rb:  s m**2 / umol -> s/m

         RS = RS*CF

  END SUBROUTINE STOMATA

!== begIN canres ===================================================================================

  SUBROUTINE CANRES (parameters,PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & !IN
                     RC    ,PSN   ,ILOC   ,JLOC  )           !OUT

! --------------------------------------------------------------------------------------------------
! calculate canopy resistance which depends on INcomINg solar radiation,
! air temperature, atmospheric water vapor pressure deficit at the
! lowest model level, and soil moisture (preferably unfrozen soil
! moisture rather than total)
! --------------------------------------------------------------------------------------------------
! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), JacquemIN and
! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
! eqns 12-14 and table 2 of sec. 3.1.2
! --------------------------------------------------------------------------------------------------
!niu    USE module_Noahlsm_utility
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! INputs

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid INdex
    INTEGER,                  INTENT(IN)  :: JLOC   !grid INdex
    REAL,                     INTENT(IN)  :: PAR    !par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: SFCTMP !canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS !surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    !water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: RCSOIL !soil moisture stress factor

!OUTputs

    REAL,                     INTENT(OUT) :: RC     !canopy resistance per unit LAI
    REAL,                     INTENT(OUT) :: PSN    !foliage photosynthesis (umolco2/m2/s)

!local

    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     !water vapor mixINg ratio (kg/kg)
    REAL                                  :: Q2SAT  !saturation Q2
    REAL                                  :: DQSDT2 !d(Q2SAT)/d(T)

! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set IN REDPRM
! ----------------------------------------------------------------------
! INitialize canopy resistance multiplier terms.
! ----------------------------------------------------------------------
    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

!  compute Q2 and Q2SAT

    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixINg ratio [kg/kg]

    CALL CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

! contribution due to INcomINg solar radiation

    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

! contribution due to air temperature

    RCT = 1.0- 0.0016* ( (parameters%TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)

! contribution due to vapor pressure deficit

    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0.,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

! determINe canopy resistance due to all factors

    RC  = parameters%RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES

!== begIN calhum ===================================================================================

        SUBROUTINE CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

  type (noahmp_parameters), INTENT(IN) :: parameters
        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622
        REAL                   :: ES, SFCPRSX

! Q2SAT: saturated mixINg ratio
        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )
! convert SFCPRS from Pa to KPa
        SFCPRSX = SFCPRS*1.E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)
! convert from  g/g to g/kg
        Q2SAT = Q2SAT * 1.E3
! Q2SAT is currently a 'mixINg ratio'

! DQSDT2 is calculated assumINg Q2SAT is a specific humidity
        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2

! DG Q2SAT needs to be IN g/g when returned for SFLX
        Q2SAT = Q2SAT / 1.E3

        END SUBROUTINE CALHUM

!== begIN tsnosoi ==================================================================================

  SUBROUTINE TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !IN
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !IN
                      SAG     ,DT      ,SNOWH   ,DZSNSO  , & !IN
                      TG      ,ILOC    ,JLOC    ,                   & !IN
                      STC     )                                       !INOUT
! --------------------------------------------------------------------------------------------------
! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
! durINg meltINg season may exceed meltINg poINt (TFRZ) but later IN PHASECHANGE
! SUBROUTINE the snow temperatures are reset to TFRZ for meltINg snow.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                         INTENT(IN)  :: ILOC
    INTEGER,                         INTENT(IN)  :: JLOC
    INTEGER,                         INTENT(IN)  :: ICE    !
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    INTEGER,                         INTENT(IN)  :: IST    !surface type

    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: TBOT   !
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SAG    !solar rad. absorbed by ground (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  !snow depth (m)
    REAL,                            INTENT(IN)  :: TG     !ground temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !layer-bot. depth from snow surf.(m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO !snow/soil layer thickness (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity (J/m3/k)

!INput and OUTput

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

!local

    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   !ZBOT from snow surface
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB !energy INflux from soil bottom (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: PHI   !light through water (w/m2)

    REAL, DIMENSION(-NSNOW+1:NSOIL) :: TBEG
    REAL                            :: ERR_EST !heat storage error  (w/m2)
    REAL                            :: SSOIL2  !ground heat flux (w/m2) (for energy check)
    REAL                            :: EFLXB2  !heat flux from the bottom (w/m2) (for energy check)
    character(len=256)              :: message
! ----------------------------------------------------------------------
! compute solar penetration through water, needs more work

    PHI(ISNOW+1:NSOIL) = 0.

! adjust ZBOT from soil surface to ZBOTSNO from snow surface

    ZBOTSNO = parameters%ZBOT - SNOWH    !from snow surface

! snow/soil heat storage for energy balance check

    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO

! compute soil temperatures

      CALL HRT   (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  STC       ) 

! update ground heat flux just for energy check, but not for fINal OUTput
! otherwise, it would break the surface energy balance

    IF(OPT_TBOT == 1) THEN
       EFLXB2  = 0.
    ELSE IF(OPT_TBOT == 2) THEN
       EFLXB2  = DF(NSOIL)*(TBOT-STC(NSOIL)) / &
            (0.5*(ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    END IF

    ! Skip the energy balance check for now, until we can make it work
    ! right for small time steps.
    return

! energy balance check

    ERR_EST = 0.0
    DO IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    ENDDO

    if (OPT_STC == 1 .OR. OPT_STC == 3) THEN   ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     ! full-implicit
       SSOIL2 = DF(ISNOW+1)*(TG-STC(ISNOW+1))/(0.5*DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2+EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.) THEN    ! W/m2
       WRITE(message,*) 'TSNOSOI is losINg(-)/gaININg(+) false energy',ERR_EST,' W/m2'
       call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            ILOC, JLOC, IST,ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       call wrf_message(trim(message))
       !niu      STOP
    END IF

  END SUBROUTINE TSNOSOI

!== begIN hrt ======================================================================================

  SUBROUTINE HRT (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOT      ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  BOTFLX    )
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! thermal diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    REAL,                            INTENT(IN)  :: TBOT   !bottom soil temp. at ZBOT (k)
    REAL,                            INTENT(IN)  :: ZBOT   !depth of lower boundary condition (m)
                                                           !from soil surface not snow surface
    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !depth of layer-bottom of snow/soil (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    !snow/soil temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity [w/m/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity [j/m3/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    !light through water (w/m2)

! OUTput

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  !right-hand side of the matrix
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     !left-hand side coefficient
    REAL,                            INTENT(OUT) :: BOTFLX !energy INflux from soil bottom (w/m2)

! local

    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1
! ----------------------------------------------------------------------

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1 .OR. OPT_STC == 3 ) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT

!== begIN hstep ====================================================================================

  SUBROUTINE HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT

! OUTput & INput
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

! local
    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN
! ----------------------------------------------------------------------

    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO

! copy values for INput variables before call to rosr12

    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO

! solve the tri-diagonal matrix equation

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

! update snow & soil temperature

    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP

!== begIN rosr12 ===================================================================================

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)
! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    P (NSOIL) = DELTA (NSOIL)
! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12

!== begIN phasechange ==============================================================================

  SUBROUTINE PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !IN
                          DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !IN
                          STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !INOUT
                          SMC     ,SH2O    ,                            & !INOUT
                          QMELT   ,IMELT   ,PONDING )                     !OUT
! ----------------------------------------------------------------------
! meltINg/freezINg of snow water and soil water
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INputs

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER, INTENT(IN)                             :: ILOC   !grid INdex
  INTEGER, INTENT(IN)                             :: JLOC   !grid INdex
  INTEGER, INTENT(IN)                             :: NSNOW  !maximum no. of snow layers [=3]
  INTEGER, INTENT(IN)                             :: NSOIL  !No. of soil layers [=4]
  INTEGER, INTENT(IN)                             :: ISNOW  !actual no. of snow layers [<=3]
  INTEGER, INTENT(IN)                             :: IST    !surface type: 1->soil; 2->lake
  REAL, INTENT(IN)                                :: DT     !land model time step (sec)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   !temporary
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: HCPCT  !heat capacity (J/m3/k)

! OUTputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  !phase change INdex
  REAL,                               INTENT(OUT) :: QMELT  !snowmelt rate [mm/s]
  REAL,                               INTENT(OUT) :: PONDING!snowmelt when snow has no layer [mm]

! INputs and OUTputs

  REAL, INTENT(INOUT) :: SNEQV
  REAL, INTENT(INOUT) :: SNOWH
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    !snow/soil layer temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   !soil liquid water [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    !total soil water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER                         :: J         !do loop INdex
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        !energy residual [w/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        !meltINg or freezINg water [kg/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      !soil/snow ice mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      !soil/snow liquid water mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL !supercooled water IN soil (kg/m2)
  REAL                            :: HEATR     !energy residual or loss after meltINg/freezINg
  REAL                            :: TEMP1     !temporary variables [kg/m2]
  REAL                            :: PROPOR
  REAL                            :: SMP       !frozen water potential (mm)
  REAL                            :: XMF       !total latent heat of phase change

! ----------------------------------------------------------------------
! Initialization

    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.

    DO J = -NSNOW+1, NSOIL
         SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       ! all layers
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               ! soil
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.
    END DO

    DO J = ISNOW+1,NSOIL       ! all layers
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO

    if(ist == 1) then
      DO J = 1,NSOIL
         IF (OPT_FRZ == 1) THEN
            IF(STC(J) < TFRZ) THEN
               SMP = HFUS*(TFRZ-STC(J))/(GRAV*STC(J))             !(m)
               SUPERCOOL(J) = parameters%SMCMAX(J)*(SMP/parameters%PSISAT(J))**(-1./parameters%BEXP(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (parameters,J,SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
         END IF
      ENDDO
    end if

    DO J = ISNOW+1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  !meltINg 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < TFRZ) THEN
             IMELT(J) = 2
         ENDIF

         ! If snow exists, but its thickness is not enough to create a layer
         IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO

! Calculate the energy surplus and loss for meltINg and freezINg

    DO J = ISNOW+1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO

! The rate of meltINg and freezINg for snow withOUT a layer, needs more work.

    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.,PROPOR * SNOWH)
        HEATR  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR > 0.) THEN
              XM(1) = HEATR*DT/HFUS             
              HM(1) = HEATR                    
        ELSE
              XM(1) = 0.
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF

! The rate of meltINg and freezINg for snow and soil

    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            IF (J <= 0) THEN                             ! snow
               MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            ELSE                                         ! soil
               IF (WMASS0(J) < SUPERCOOL(J)) THEN
                  MICE(J) = 0.
               ELSE
                  MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J),WICE0(J)-XM(J))
                  MICE(J) = MAX(MICE(J),0.0)
               ENDIF
            ENDIF
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR
            IF (J <= 0) THEN                             ! snow
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
               IF (MICE(J) == 0.) THEN         ! BARLAGE
                  STC(J) = TFRZ                ! BARLAGE
                  HM(J+1) = HM(J+1) + HEATR    ! BARLAGE
                  XM(J+1) = HM(J+1)*DT/HFUS    ! BARLAGE
               ENDIF 
            END IF
         ENDIF

         XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO

    DO J = ISNOW+1,0             ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              ! soil
       SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * DZSNSO(J))
    END DO
   
  END SUBROUTINE PHASECHANGE

!== begIN frh2o ====================================================================================

  SUBROUTINE FRH2O (parameters,ISOIL,FREE,TKELV,SMC,SH2O)

! ----------------------------------------------------------------------
! SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (TFRZ).  REQUIRES NEWTON-TYPE ITERATION
! TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE TFRZ.
! ----------------------------------------------------------------------
! INPUT:

!   TKELV.........TEMPERATURE (KelvIN)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSISAT........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)

! OUTPUT:
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT [m3/m3]
! ----------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,INTENT(IN)   :: ISOIL
    REAL, INTENT(IN)     :: SH2O,SMC,TKELV
    REAL, INTENT(OUT)    :: FREE
    REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
    INTEGER              :: NLOG,KCOUNT
!      PARAMETER(CK = 0.0)
    REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
         DICE = 920.0
    CHARACTER(LEN=80)    :: message

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
    BX = parameters%BEXP(ISOIL)
! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

    IF (parameters%BEXP(ISOIL) >  BLIM) BX = BLIM
    NLOG = 0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (TFRZ), SH2O = SMC
! ----------------------------------------------------------------------
    KCOUNT = 0
    IF (TKELV > (TFRZ- 1.E-3)) THEN
       FREE = SMC
    ELSE

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
       IF (CK /= 0.0) THEN
          SWL = SMC - SH2O
! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
          IF (SWL > (SMC -0.02)) SWL = SMC -0.02
! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
          IF (SWL < 0.) SWL = 0.
1001      ContINue
          IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
          NLOG = NLOG +1
          DF = ALOG ( ( parameters%PSISAT(ISOIL) * GRAV / HFUS ) * ( ( 1. + CK * SWL )**2.) * &
               ( parameters%SMCMAX(ISOIL) / (SMC - SWL) )** BX) - ALOG ( - (               &
               TKELV - TFRZ)/ TKELV)
          DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF / DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
          IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
          IF (SWLK < 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
          DSWL = ABS (SWLK - SWL)
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
          SWL = SWLK
          IF ( DSWL <= ERROR ) THEN
             KCOUNT = KCOUNT +1
          END IF
! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
          goto 1001
1002      contINue
          FREE = SMC - SWL
       END IF
! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
       IF (KCOUNT == 0) THEN
          write(message, '("FlerchINger used IN NEW version. Iterations=", I6)') NLOG
          call wrf_message(trim(message))
          FK = ( ( (HFUS / (GRAV * ( - parameters%PSISAT(ISOIL))))*                    &
               ( (TKELV - TFRZ)/ TKELV))** ( -1/ BX))* parameters%SMCMAX(ISOIL)
          IF (FK < 0.02) FK = 0.02
          FREE = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
       END IF
    END IF
! ----------------------------------------------------------------------
  END SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! ==================================================================================================
! **********************End of energy SUBROUTINEs***********************
! ==================================================================================================

!== begIN water ====================================================================================

  SUBROUTINE WATER (parameters,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !IN
                    VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !IN
                    ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !IN
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC   ,JLOC ,SMCEQ , & !IN
                    BDFALL ,FP     ,RAIN   ,SNOW,                    & !IN  MB/AN: v3.7
		    QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,    & !IN  MB
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !INOUT
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !INOUT
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !INOUT
                    SMCWTD ,DEEPRECH,RECH                          , & !INOUT
                    CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !OUT
                    QIN    ,QDIS   ,PONDING1       ,PONDING2,        &
                    QSNBOT                                           &
#ifdef WRF_HYDRO
                        ,sfcheadrt                     &
#endif
                    )  !OUT
! ----------------------------------------------------------------------  
! Code history:
! Initial code: Guo-Yue Niu, Oct. 2007
! ----------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN)    :: ILOC    !grid INdex
  INTEGER,                         INTENT(IN)    :: JLOC    !grid INdex
  INTEGER,                         INTENT(IN)    :: VEGTYP  !vegetation type
  INTEGER,                         INTENT(IN)    :: NSNOW   !maximum no. of snow layers
  INTEGER                        , INTENT(IN)    :: IST     !surface type 1-soil; 2-lake
  INTEGER,                         INTENT(IN)    :: NSOIL   !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   !meltINg state INdex [1-melt; 2-freeze]
  REAL,                            INTENT(IN)    :: DT      !maIN time step (s)
  REAL,                            INTENT(IN)    :: UU      !u-direction wINd speed [m/s]
  REAL,                            INTENT(IN)    :: VV      !v-direction wINd speed [m/s]
  REAL,                            INTENT(IN)    :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  REAL,                            INTENT(IN)    :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL,                            INTENT(IN)    :: QPRECC  !convective precipitation (mm/s)
  REAL,                            INTENT(IN)    :: QPRECL  !large-scale precipitation (mm/s)
  REAL,                            INTENT(IN)    :: ELAI    !leaf area INdex, after buryINg by snow
  REAL,                            INTENT(IN)    :: ESAI    !stem area INdex, after buryINg by snow
  REAL,                            INTENT(IN)    :: SFCTMP  !surface air temperature [k]
  REAL,                            INTENT(IN)    :: QVAP    !soil surface evaporation rate[mm/s]
  REAL,                            INTENT(IN)    :: QDEW    !soil surface dew rate[mm/s]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL   !depth of layer-bottom from soil surface
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: BTRANI  !soil water stress factor (0 to 1)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD !ice fraction at last timestep
!  REAL                           , INTENT(IN)    :: PONDING ![mm]
  REAL                           , INTENT(IN)    :: TG      !ground temperature (k)
  REAL                           , INTENT(IN)    :: FVEG    !greeness vegetation fraction (-)
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: FP       !fraction of the gridcell that receives precipitation ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: RAIN     !raINfall (mm/s) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: SNOW     !snowfall (mm/s) ! MB/AN: v3.7
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ   !equilibrium soil water content [m3/m3] (used IN m-m&f groundwater dynamics)
  REAL                           , INTENT(IN)    :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           , INTENT(IN)    :: QRAIN   !raIN at ground srf (mm) [+]
  REAL                           , INTENT(IN)    :: SNOWHIN !snow depth INcreasINg rate (m/s)

! INput/OUTput
  INTEGER,                         INTENT(INOUT) :: ISNOW   !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: CANLIQ  !INtercepted liquid water (mm)
  REAL,                            INTENT(INOUT) :: CANICE  !INtercepted ice mass (mm)
  REAL,                            INTENT(INOUT) :: TV      !vegetation temperature (k)
  REAL,                            INTENT(INOUT) :: SNOWH   !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV   !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    !soil liquid water content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC     !total soil water content [m3/m3]
  REAL,                            INTENT(INOUT) :: ZWT     !the depth to water table [m]
  REAL,                            INTENT(INOUT) :: WA      !water storage IN aquifer [mm]
  REAL,                            INTENT(INOUT) :: WT      !water storage IN aquifer 
                                                            !+ stuarated soil [mm]
  REAL,                            INTENT(INOUT) :: WSLAKE  !water storage IN lake (can be -) (mm)
  REAL                           , INTENT(INOUT) :: PONDING ![mm]
  REAL,                            INTENT(INOUT) :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
  REAL,                            INTENT(INOUT) :: DEEPRECH !recharge to or from the water table when deep [m]
  REAL,                            INTENT(INOUT) :: RECH !recharge to or from the water table when shallow [m] (diagnostic)

! OUTput
  REAL,                            INTENT(OUT)   :: CMC     !INtercepted water per ground area (mm)
  REAL,                            INTENT(OUT)   :: ECAN    !evap of INtercepted water (mm/s) [+]
  REAL,                            INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL,                            INTENT(OUT)   :: FWET    !wetted/snowed fraction of canopy (-)
  REAL,                            INTENT(OUT)   :: RUNSRF  !surface runoff [mm/s] 
  REAL,                            INTENT(OUT)   :: RUNSUB  !baseflow (sturation excess) [mm/s]
  REAL,                            INTENT(OUT)   :: QIN     !groundwater recharge [mm/s]
  REAL,                            INTENT(OUT)   :: QDIS    !groundwater discharge [mm/s]
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  !meltINg water OUT of snow bottom [mm/s]
  REAL                              , INTENT(IN)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(IN)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(IN)   :: FROZEN_GROUND ! used to defINe latent heat pathway
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to defINe latent heat pathway


! local
  INTEGER                                        :: IZ
  REAL                                           :: QINSUR  !water INput on soil surface [m/s]
  REAL                                           :: QSEVA   !soil surface evap rate [mm/s]
  REAL                                           :: QSDEW   !soil surface dew rate [mm/s]
  REAL                                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: ETRANI  !transpiration rate (mm/s) [+]
  REAL, DIMENSION(       1:NSOIL)                :: WCND   !hydraulic conductivity (m/s)
  REAL                                           :: QDRAIN  !soil-bottom free draINage [mm/s] 
  REAL                                           :: SNOFLOW !glacier flow [mm/s]
  REAL                                           :: FCRMAX !maximum of FCR (-)

  REAL, PARAMETER ::  WSLMAX = 5000.      !maximum lake water storage (mm)

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt
#endif

! ----------------------------------------------------------------------
! INitialize

   ETRANI(1:NSOIL) = 0.
   SNOFLOW         = 0.
   RUNSUB          = 0.
   QINSUR          = 0.

! canopy-INtercepted snowfall/raINfall, drips, and throughfall

   CALL CANWATER (parameters,VEGTYP ,DT     , & !IN
                  FCEV   ,FCTR   ,ELAI   , & !IN
                  ESAI   ,TG     ,FVEG   ,ILOC   , JLOC, & !IN
                  BDFALL ,FROZEN_CANOPY  , & !IN     
                  CANLIQ ,CANICE ,TV     ,                 & !INOUT
                  CMC    ,ECAN   ,ETRAN  , & !OUT
                  FWET      )                           !OUT

! sublimation, frost, evaporation, and dew

     QSNSUB = 0.
     IF (SNEQV > 0.) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB

     QSNFRO = 0.
     IF (SNEQV > 0.) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO

     CALL SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !IN
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !IN
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !IN
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !INOUT
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !INOUT
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !OUT

   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

! convert units (mm/s -> m/s)

    !PONDING: meltINg water from snow when there is no layer
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001
!    QINSUR = PONDING/DT * 0.001

    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF

    QSEVA  = QSEVA * 0.001 

    DO IZ = 1, parameters%NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

#ifdef WRF_HYDRO
       QINSUR = QINSUR+sfcheadrt/DT*0.001  !sfcheadrt units (m)
#endif

! lake/soil water balances

    IF (IST == 2) THEN                                        ! lake
       RUNSRF = 0.
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.             !mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.*DT -RUNSRF*DT   !mm
    ELSE                                                      ! soil
       CALL      SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !IN
                            QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC , & !IN
                            SH2O   ,SMC    ,ZWT    ,VEGTYP , & !INOUT
                           SMCWTD, DEEPRECH                       , & !INOUT
                            RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !OUT
 
       IF(OPT_RUN == 1) THEN 
          CALL GROUNDWATER (parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !IN
                            STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !IN
                            SH2O   ,ZWT    ,WA     ,WT     ,         & !INOUT
                            QIN    ,QDIS   )                           !OUT
          RUNSUB       = QDIS          !mm/s
       END IF

       IF(OPT_RUN == 3 .or. OPT_RUN == 4) THEN 
          RUNSUB       = RUNSUB + QDRAIN        !mm/s
       END IF

       DO IZ = 1,NSOIL
           SMC(IZ) = SH2O(IZ) + SICE(IZ)
       ENDDO
 
       IF(OPT_RUN == 5) THEN
          CALL SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL, ZSOIL, DT       , & !IN
                         DZSNSO ,SMCEQ   ,ILOC , JLOC        , & !IN
                         SMC    ,ZWT    ,SMCWTD ,RECH, QDRAIN  ) !INOUT

          SH2O(NSOIL) = SMC(NSOIL) - SICE(NSOIL)
          RUNSUB = RUNSUB + QDRAIN !it REALly comes from SUBROUTINE watertable, which is not called with the same frequency as the soil rOUTINes here
          WA = 0.
       ENDIF

    ENDIF

    RUNSUB       = RUNSUB + SNOFLOW         !mm/s

  END SUBROUTINE WATER

!== begIN canwater =================================================================================

  SUBROUTINE CANWATER (parameters,VEGTYP ,DT     , & !IN
                       FCEV   ,FCTR   ,ELAI   , & !IN
                       ESAI   ,TG     ,FVEG   ,ILOC   , JLOC , & !IN
                       BDFALL ,FROZEN_CANOPY  ,  & !IN      
                       CANLIQ ,CANICE ,TV     ,                 & !INOUT
                       CMC    ,ECAN   ,ETRAN  , & !OUT
                       FWET      )                           !OUT

! ------------------------ code history ------------------------------
! canopy hydrology
! --------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ INput/OUTput variables --------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid INdex
  INTEGER,INTENT(IN)  :: JLOC    !grid INdex
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  REAL,   INTENT(IN)  :: DT      !maIN time step (s)
  REAL,   INTENT(IN)  :: FCEV    !canopy evaporation (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: FCTR    !transpiration (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: ELAI    !leaf area INdex, after buryINg by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area INdex, after buryINg by snow
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to defINe latent heat pathway
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7

! INput & OUTput
  REAL, INTENT(INOUT) :: CANLIQ  !INtercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !INtercepted ice mass (mm)
  REAL, INTENT(INOUT) :: TV      !vegetation temperature (k)

! OUTput
  REAL, INTENT(OUT)   :: CMC     !INtercepted water (mm)
  REAL, INTENT(OUT)   :: ECAN    !evaporation of INtercepted water (mm/s) [+]
  REAL, INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow INterception (mm)
  REAL                :: MAXLIQ  !canopy capacity for raIN INterception (mm)
  REAL                :: QEVAC   !evaporation rate (mm/s)
  REAL                :: QDEWC   !dew rate (mm/s)
  REAL                :: QFROC   !frost rate (mm/s)
  REAL                :: QSUBC   !sublimation rate (mm/s)
  REAL                :: QMELTC  !meltINg rate of canopy snow (mm/s)
  REAL                :: QFRZC   !refreezINg rate of canopy liquid water (mm/s)
  REAL                :: CANMAS  !total canopy mass (kg/m2)
! --------------------------------------------------------------------
! INitialization

      ECAN    = 0.0

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! evaporation, transpiration, and dew

      IF (.NOT.FROZEN_CANOPY) THEN             ! Barlage: change to frozen_canopy
        ETRAN = MAX( FCTR/HVAP, 0. )
        QEVAC = MAX( FCEV/HVAP, 0. )
        QDEWC = ABS( MIN( FCEV/HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
      ELSE
        ETRAN = MAX( FCTR/HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( FCEV/HSUB, 0. )
        QFROC = ABS( MIN( FCEV/HSUB, 0. ) )
      ENDIF

! canopy water balance. for convenience allow dew to brINg CANLIQ above
! maxh2o or else would have to re-adjust drip

       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.,CANLIQ+(QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      QSUBC = MIN(CANICE/DT,QSUBC) 
      CANICE= MAX(0.,CANICE + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.
     
! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! phase change

      QMELTC = 0.
      QFRZC = 0.

      IF(CANICE.GT.1.E-6.AND.TV.GT.TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-TFRZ)*CICE*CANICE/DENICE/(DT*HFUS))
         CANICE = MAX(0.,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.,CANLIQ + QMELTC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(TFRZ-TV)*CWAT*CANLIQ/DENH2O/(DT*HFUS))
         CANLIQ = MAX(0.,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.,CANICE + QFRZC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

! total canopy water

      CMC = CANLIQ + CANICE

! total canopy evaporation

      ECAN = QEVAC + QSUBC - QDEWC - QFROC

  END SUBROUTINE CANWATER

!== begIN snowwater ================================================================================

  SUBROUTINE SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !IN
                        SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !IN
                        QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !IN
                        ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !INOUT
                        SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !INOUT
                        QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !OUT
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN)    :: ILOC   !grid INdex
  INTEGER,                         INTENT(IN)    :: JLOC   !grid INdex
  INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !meltINg state INdex [0-no melt;1-melt]
  REAL,                            INTENT(IN)    :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil surface
  REAL,                            INTENT(IN)    :: SFCTMP !surface air temperature [k]
  REAL,                            INTENT(IN)    :: SNOWHIN!snow depth INcreasINg rate (m/s)
  REAL,                            INTENT(IN)    :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
  REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
  REAL,                            INTENT(IN)    :: QRAIN  !snow surface raIN rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! INput & OUTput
  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !snow/soil layer thickness [m]

! OUTput
  REAL,                              INTENT(OUT) :: QSNBOT !meltINg water OUT of snow bottom [mm/s]
  REAL,                              INTENT(OUT) :: SNOFLOW!glacier flow [mm]
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2

! local
  INTEGER :: IZ,i
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------
   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & !IN
                  SFCTMP ,ILOC   ,JLOC   ,                 & !IN
                  ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & !INOUT
                  SNLIQ  ,SNEQV  )                           !INOUT

! MB: do each if block separately

   IF(ISNOW < 0) &        ! when multi-layer
   CALL  COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !IN
                  SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC ,& !IN
                  ISNOW  ,DZSNSO ,ZSNSO  )                   !INOUT

   IF(ISNOW < 0) &        !when multi-layer
   CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !IN
                  ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !INOUT
                  DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !INOUT
                  PONDING1       ,PONDING2)                  !OUT

   IF(ISNOW < 0) &        !when multi-layer
   CALL   DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !IN
                  ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   !INOUT

   CALL  SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !IN 
                  QRAIN  ,ILOC   ,JLOC   ,                 & !IN
                  ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !INOUT
                  SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !INOUT
                  QSNBOT ,PONDING1       ,PONDING2)           !OUT

!set empty snow layers to zero

   do iz = -nsnow+1, isnow
        snice(iz) = 0.
        snliq(iz) = 0.
        stc(iz)   = 0.
        dzsnso(iz)= 0.
        zsnso(iz) = 0.
   enddo

!to obtaIN equilibrium state of snow IN glacier region
       
   IF(SNEQV > 5000.) THEN   ! 5000 mm -> maximum water depth
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 5000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow

   IF(ISNOW < 0) THEN  ! MB: only do for multi-layer
       SNEQV = 0.
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF

! Reset ZSNSO and layer thINkness DZSNSO

   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER

!== begIN snowfall =================================================================================

  SUBROUTINE SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & !IN
                       SFCTMP ,ILOC   ,JLOC   ,                  & !IN
                       ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & !INOUT
                       SNLIQ  ,SNEQV  )                            !INOUT
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                            INTENT(IN) :: ILOC   !grid INdex
  INTEGER,                            INTENT(IN) :: JLOC   !grid INdex
  INTEGER,                            INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                            INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                               INTENT(IN) :: DT     !maIN time step (s)
  REAL,                               INTENT(IN) :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                               INTENT(IN) :: SNOWHIN!snow depth INcreasINg rate (m/s)
  REAL,                               INTENT(IN) :: SFCTMP !surface air temperature [k]

! INput and OUTput

  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow depth [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !swow water equivalent [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !thickness of snow/soil layers (m)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER :: NEWNODE            ! 0-no new layers, 1-creatINg new layers
! ----------------------------------------------------------------------
    NEWNODE  = 0

! shallow snow / no layer

    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF

! creatINg a new layer
 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.025) THEN !MB: change limit
!    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   ! temporary setup
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.
    END IF

! snow with layers

    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWFALL

!== begIN combINe ==================================================================================

  SUBROUTINE COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !IN
                      ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !INOUT
                      DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !INOUT
                      PONDING1       ,PONDING2)                  !OUT
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER, INTENT(IN)     :: ILOC
    INTEGER, INTENT(IN)     :: JLOC
    INTEGER, INTENT(IN)     :: NSNOW                        !maximum no. of snow layers
    INTEGER, INTENT(IN)     :: NSOIL                        !no. of soil layers

! INput and OUTput

    INTEGER,                         INTENT(INOUT) :: ISNOW !actual no. of snow layers
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  !soil liquid moisture (m3/m3)
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  !soil ice moisture (m3/m3)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]
    REAL,                            INTENT(INOUT) :: sneqv !snow water equivalent [m]
    REAL,                            INTENT(INOUT) :: snowh !snow depth [m]
    REAL,                            INTENT(OUT) :: PONDING1
    REAL,                            INTENT(OUT) :: PONDING2

! local variables:

    INTEGER :: I,J,K,L               ! node INdices
    INTEGER :: ISNOW_OLD             ! number of top snow layer
    INTEGER :: MSSI                  ! node INdex
    INTEGER :: NEIBOR                ! adjacent node selected for combINation
    REAL    :: ZWICE                 ! total ice mass IN snow
    REAL    :: ZWLIQ                 ! total liquid water IN snow

    REAL    :: DZMIN(3)              ! mINimum of top snow layer
!    DATA DZMIN /0.045, 0.05, 0.2/
    DATA DZMIN /0.025, 0.025, 0.1/  ! MB: change limit
!-----------------------------------------------------------------------

       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
               ELSE
	         IF(SNICE(J) >= 0.) THEN
                  PONDING1 = SNLIQ(J)    ! ISNOW WILL GET SET TO ZERO BELOW; PONDING1 WILL GET 
                  SNEQV = SNICE(J)       ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                  SNOWH = DZSNSO(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
		 ELSE   ! SNICE OVER-SUBLIMATED EARLIER
		  PONDING1 = SNLIQ(J) + SNICE(J)
		  IF(PONDING1 < 0.) THEN  ! IF SNICE AND SNLIQ SUBLIMATES REMOVE FROM SOIL
		   SICE(1) = MAX(0.0,SICE(1)+PONDING1/(DZSNSO(1)*1000.))
                   PONDING1 = 0.0
		  END IF
                  SNEQV = 0.0
                  SNOWH = 0.0
		 END IF
                 SNLIQ(J) = 0.0
                 SNICE(J) = 0.0
                 DZSNSO(J) = 0.0
               ENDIF
!                SH2O(1) = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
!                SICE(1) = SICE(1)+SNICE(J)/(DZSNSO(1)*1000.)
             ENDIF

             ! shift all elements above this down by one.
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO

! to conserve water IN case of too large surface sublimation

       IF(SICE(1) < 0.) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.
       END IF

       IF(ISNOW ==0) RETURN   ! MB: get OUT if no longer multi-layer

       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO

! check the snow depth - all snow gone
! the liquid water assumes pondINg on soil surface.

       IF (SNOWH < 0.025 .AND. ISNOW < 0 ) THEN ! MB: change limit
!       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = ZWLIQ           ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
          IF(SNEQV <= 0.) SNOWH = 0. ! SHOULD BE ZERO; SEE ABOVE
       END IF

!       IF (SNOWH < 0.05 ) THEN
!          ISNOW  = 0
!          SNEQV = ZWICE
!          SH2O(1) = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)
!          IF(SNEQV <= 0.) SNOWH = 0.
!       END IF

! check the snow depth - snow layers combINed

       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF

                ! Node l and j are combINed and stored as node j.
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO (parameters,DZSNSO(J), SNLIQ(J), SNICE(J), &
                   STC(J), DZSNSO(L), SNLIQ(L), SNICE(L), STC(L) )

                ! Now shift all elements above this down one.
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                ! Decrease the number of snow layers
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                ! The layer thickness is greater than the prescribed mINimum value
                MSSI = MSSI + 1

             END IF
          END DO

       END IF

  END SUBROUTINE COMBINE

!== begIN divide ===================================================================================

  SUBROUTINE DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !IN
                     ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO  )  !INOUT
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER, INTENT(IN)                            :: NSNOW !maximum no. of snow layers [ =3]
    INTEGER, INTENT(IN)                            :: NSOIL !no. of soil layers [ =4]

! INput and OUTput

    INTEGER                        , INTENT(INOUT) :: ISNOW !actual no. of snow layers 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]

! local variables:

    INTEGER                                        :: J     !INdices
    INTEGER                                        :: MSNO  !number of layer (top) to MSNO (bot)
    REAL                                           :: DRR   !thickness of the combINed [m]
    REAL, DIMENSION(       1:NSNOW)                :: DZ    !snow layer thickness [m]
    REAL, DIMENSION(       1:NSNOW)                :: SWICE !partial volume of ice [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ !partial volume of liquid water [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  !node temperature [k]
    REAL                                           :: ZWICE !temporary
    REAL                                           :: ZWLIQ !temporary
    REAL                                           :: PROPOR!temporary
    REAL                                           :: DTDZ  !temporary
! ----------------------------------------------------------------------

    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          ! Specify a new snow layer
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05

             CALL COMBO (parameters,DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             ! subdivide a new layer
             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  ! MB: change limit
!             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF

             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO (parameters,DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
    END DO


!    DO J = ISNOW+1,NSOIL
!    WRITE(*,'(I5,7F10.3)') J, DZSNSO(J), SNICE(J), SNLIQ(J),STC(J)
!    END DO

  END SUBROUTINE DIVIDE

!== begIN combo ====================================================================================

  SUBROUTINE COMBO(parameters,DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------s
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    REAL, INTENT(IN)    :: DZ2   !nodal thickness of 2 elements beINg combINed [m]
    REAL, INTENT(IN)    :: WLIQ2 !liquid water of element 2 [kg/m2]
    REAL, INTENT(IN)    :: WICE2 !ice of element 2 [kg/m2]
    REAL, INTENT(IN)    :: T2    !nodal temperature of element 2 [k]
    REAL, INTENT(INOUT) :: DZ    !nodal thickness of 1 elements beINg combINed [m]
    REAL, INTENT(INOUT) :: WLIQ  !liquid water of element 1
    REAL, INTENT(INOUT) :: WICE  !ice of element 1 [kg/m2]
    REAL, INTENT(INOUT) :: T     !node temperature of element 1 [k]

! local 

    REAL                :: DZC   !total thickness of nodes 1 and 2 (DZC=DZ+DZ2).
    REAL                :: WLIQC !combINed liquid water [kg/m2]
    REAL                :: WICEC !combINed ice [kg/m2]
    REAL                :: TC    !combINed node temperature [k]
    REAL                :: H     !enthalpy of element 1 [J/m2]
    REAL                :: H2    !enthalpy of element 2 [J/m2]
    REAL                :: HC    !temporary

!-----------------------------------------------------------------------

    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO

!== begIN compact ==================================================================================

  SUBROUTINE COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !IN
                      SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC , & !IN
                      ISNOW  ,DZSNSO ,ZSNSO )                    !INOUT
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid INdex
   INTEGER,                         INTENT(IN)    :: JLOC   !grid INdex
   INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers [ =4]
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers [ =3]
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !meltINg state INdex [0-no melt;1-melt]
   REAL,                            INTENT(IN)    :: DT     !time step (sec)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil srf
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! INput and OUTput
   INTEGER,                         INTENT(INOUT) :: ISNOW  ! actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer thickness [m]
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  ! depth of snow/soil layer-bottom

! local
   REAL, PARAMETER     :: C2 = 21.e-3   ![m3/kg] ! default 21.e-3
   REAL, PARAMETER     :: C3 = 2.5e-6   ![1/s]  
   REAL, PARAMETER     :: C4 = 0.04     ![1/k]
   REAL, PARAMETER     :: C5 = 2.0      !
   REAL, PARAMETER     :: DM = 100.0    !upper Limit on destructive metamorphism compaction [kg/m3]
   REAL, PARAMETER     :: ETA0 = 0.8e+6 !viscosity coefficient [kg-s/m2] 
                                        !accordINg to Anderson, it is between 0.52e6~1.38e6
   REAL :: BURDEN !pressure of overlyINg snow [kg/m2]
   REAL :: DDZ1   !rate of settlINg of snow pack due to destructive metamorphism.
   REAL :: DDZ2   !rate of compaction of snow pack due to overburden.
   REAL :: DDZ3   !rate of compaction of snow pack due to melt [1/s]
   REAL :: DEXPF  !EXPF=exp(-c4*(273.15-STC)).
   REAL :: TD     !STC - TFRZ [K]
   REAL :: PDZDTC !nodal rate of change IN fractional-thickness due to compaction [fraction/s]
   REAL :: VOID   !void (1 - SNICE - SNLIQ)
   REAL :: WX     !water mass (ice + liquid) [kg/m2]
   REAL :: BI     !partial density of ice [kg/m3]
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   !fraction of ice at current time step

   INTEGER  :: J

! ----------------------------------------------------------------------
    BURDEN = 0.0

    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1. - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           ! SettlINg as a result of destructive metamorphism

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           ! Liquid water term

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           ! Compaction due to overburden

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 ! 0.5*WX -> self-burden

           ! Compaction occurrINg durINg melt

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           ! sometimes too large
           ELSE
              DDZ3 = 0.
           END IF

           ! Time rate of fractional change IN DZ (units of s-1)

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           ! The change IN DZ due to compaction

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
        END IF

        ! Pressure of overlyINg snow

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT

!== begIN snowh2o ==================================================================================

  SUBROUTINE SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !IN 
                      QRAIN  ,ILOC   ,JLOC   ,                 & !IN
                      ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !INOUT
                      SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !INOUT
                      QSNBOT ,PONDING1       ,PONDING2)          !OUT
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resultINg from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid INdex
   INTEGER,                         INTENT(IN)    :: JLOC   !grid INdex
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers[=3]
   INTEGER,                         INTENT(IN)    :: NSOIL  !No. of soil layers[=4]
   REAL,                            INTENT(IN)    :: DT     !time step
   REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
   REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
   REAL,                            INTENT(IN)    :: QRAIN  !snow surface raIN rate[mm/s]

! OUTput

   REAL,                            INTENT(OUT)   :: QSNBOT !meltINg water OUT of snow bottom [mm/s]

! INput and OUTput

   INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer depth [m]
   REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
   REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]

! local variables:

   INTEGER                     :: J         !do loop/array INdices
   REAL                        :: QIN       !water flow INto the element (mm/s)
   REAL                        :: QOUT      !water flow OUT of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after mINus sublimation
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   !partial volume of liquid water IN layer
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   !partial volume of ice lens IN layer
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
   REAL :: PONDING1, PONDING2
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'

   IF(SNEQV == 0.) THEN
      SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)  ! Barlage: SH2O->SICE v3.6
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

! for shallow snow withOUT a layer
! snow surface sublimation may be larger than existINg snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.

   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.,PROPOR * SNOWH)

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF

! for deep snow

   IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent OUT-of-bounds array references

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC, JLOC   , & !IN
              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !INOUT
              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !INOUT
              PONDING1, PONDING2 )                       !OUT
      ENDIF
      !KWM:  SubrOUTINe COMBINE can change ISNOW to make it 0 agaIN?
      IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent OUT-of-bounds array references
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the SUBROUTINE (Just set QSNBOT=0)?

! Porosity and partial volume

   DO J = ISNOW+1, 0
     VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
     EPORE(J)        = 1. - VOL_ICE(J)
   END DO

   QIN = 0.
   QOUT = 0.

   DO J = ISNOW+1, 0
     SNLIQ(J) = SNLIQ(J) + QIN
     VOL_LIQ(J) = SNLIQ(J)/(DZSNSO(J)*DENH2O)
     QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))
     QOUT = QOUT*DENH2O
     SNLIQ(J) = SNLIQ(J) - QOUT
     QIN = QOUT
   END DO

! Liquid water from snow bottom to soil

   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O

!== begIN soilwater ================================================================================

  SUBROUTINE SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !IN
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !IN
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,& !INOUT
                        SMCWTD, DEEPRECH                       ,& !INOUT
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !OUT

! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                     INTENT(IN) :: ILOC   !grid INdex
  INTEGER,                     INTENT(IN) :: JLOC   !grid INdex
  INTEGER,                     INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                     INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                        INTENT(IN) :: DT     !time step (sec)
  REAL, INTENT(IN)                        :: QINSUR !water INput on soil surface [mm/s]
  REAL, INTENT(IN)                        :: QSEVA  !evap from soil surface [mm/s]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ETRANI !evapotranspiration from soil layers [mm/s]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SICE   !soil ice content [m3/m3]

  INTEGER,                     INTENT(IN) :: VEGTYP

! INput & OUTput
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]
  REAL,                     INTENT(INOUT) :: SMCWTD !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL                    , INTENT(INOUT) :: DEEPRECH

! OUTput
  REAL, INTENT(OUT)                       :: QDRAIN !soil-bottom free draINage [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSRF !surface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSUB !subsurface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: FCRMAX !maximum of FCR (-)
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   !hydraulic conductivity (m/s)

! local
  INTEGER                                 :: K,IZ   !do-loop INdex
  INTEGER                                 :: ITER   !iteration INdex
  REAl                                    :: DTFINE !fINe time step (s)
  REAL, DIMENSION(1:NSOIL)                :: RHSTT  !right-hand side term of the matrix
  REAL, DIMENSION(1:NSOIL)                :: AI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: BI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: CI     !left-hand side term

  REAL                                    :: FFF    !runoff decay factor (m-1)
  REAL                                    :: RSBMX  !baseflow coefficient [mm/s]
  REAL                                    :: PDDUM  !INfiltration rate at surface (m/s)
  REAL                                    :: FICE   !ice fraction IN frozen soil
  REAL                                    :: WPLUS  !saturation excess of the total soil [m]
  REAL                                    :: RSAT   !accumulation of WPLUS (saturation excess) [m]
  REAL                                    :: SICEMAX!maximum soil ice content (m3/m3)
  REAL                                    :: SH2OMIN!mINimum soil liquid water content (m3/m3)
  REAL                                    :: WTSUB  !sum of WCND(K)*DZSNSO(K)
  REAL                                    :: MH2O   !water mass removal (mm)
  REAL                                    :: FSAT   !fractional saturated area (-)
  REAL, DIMENSION(1:NSOIL)                :: MLIQ   !
  REAL                                    :: XS     !
  REAL                                    :: WATMIN !
  REAL                                    :: QDRAIN_SAVE !
  REAL                                    :: RUNSRF_SAVE !
  REAL                                    :: EPORE  !effective porosity [m3/m3]
  REAL, DIMENSION(1:NSOIL)                :: FCR    !impermeable fraction due to frozen soil
  INTEGER                                 :: NITER  !iteration times soil moisture (-)
  REAL                                    :: SMCTOT !2-m averaged soil moisture (m3/m3)
  REAL                                    :: DZTOT  !2-m soil depth (m)
  REAL, PARAMETER :: A = 4.0
! ----------------------------------------------------------------------
    RUNSRF = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0

! for the case when snowmelt water is too large

    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
       RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
       SH2O(K) = MIN(EPORE,SH2O(K))             
    END DO

!impermeable fraction due to frozen soil

    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/parameters%SMCMAX(K))
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

! maximum soil ice content and mINimum liquid water of all layers

    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = parameters%SMCMAX(1)
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO

!subsurface runoff for runoff scheme option 2

    IF(OPT_RUN == 2) THEN 
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-parameters%TIMEAN) * EXP(-FFF*ZWT)   ! mm/s
    END IF

!surface runoff and INfiltration rate usINg different schemes

!jref impermable surface at urban
    IF ( parameters%urban_flag ) FCR(1)= 0.95

    IF(OPT_RUN == 1) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*(ZWT-2.0))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !IN
                   SICEMAX,QINSUR ,                         & !IN
                   PDDUM  ,RUNSRF )                           !OUT
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)/parameters%SMCMAX(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT) ** 4.        !BATS

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

! determINe iteration times and fINer time step

    NITER = 1

!    IF(OPT_INF == 1) THEN    !OPT_INF =2 may cause water imbalance
       NITER = 3
       IF (PDDUM*DT>DZSNSO(1)*parameters%SMCMAX(1) ) THEN
          NITER = NITER*2
       END IF
!    END IF                 

    DTFINE  = DT / NITER

! solve soil moisture

    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0
    DO ITER = 1, NITER
       IF(QINSUR > 0. .and. OPT_RUN == 3) THEN
          CALL INFIL (parameters,NSOIL  ,DTFINE     ,ZSOIL  ,SH2O   ,SICE   , & !IN
                      SICEMAX,QINSUR ,                         & !IN
                      PDDUM  ,RUNSRF )                           !OUT
       END IF

       CALL SRT   (parameters,NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & !IN
                   QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !IN
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !IN
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !OUT
                   WCND   )                                   !OUT
  
       CALL SSTEP (parameters,NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & !IN
                   SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !IN
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & !INOUT
                   RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !INOUT
                   WPLUS)                                     !OUT
       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF
    END DO

    QDRAIN = QDRAIN_SAVE/NITER
    RUNSRF = RUNSRF_SAVE/NITER

    RUNSRF = RUNSRF * 1000. + RSAT * 1000./DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.

!WRF_HYDRO_DJG...
!yw    INFXSRT = RUNSRF * DT   !mm/s -> mm

! removal of soil water due to groundwater flow (option 2)

    IF(OPT_RUN == 2) THEN
         WTSUB = 0.
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO

         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       ! mm
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.)
         END DO
    END IF

! Limit MLIQ to be greater than or equal to watmIN.
! Get water needed to brINg MLIQ equal WATMIN from lower layer.

   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF

  END SUBROUTINE SOILWATER

!== begIN zwteq ====================================================================================

  SUBROUTINE ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]

! OUTput

  REAL,                           INTENT(OUT) :: ZWT    !water table depth [m]

! locals

  INTEGER :: K                      !do-loop INdex
  INTEGER, PARAMETER :: NFINE = 100 !no. of fINe soil layers of 6m soil
  REAL    :: WD1                    !water deficit from coarse (4-L) soil moisture profile
  REAL    :: WD2                    !water deficit from fINe (100-L) soil moisture profile
  REAL    :: DZFINE                 !layer thickness of the 100-L soil layers to 6.0 m
  REAL    :: TEMP                   !temporary variable
  REAL, DIMENSION(1:NFINE) :: ZFINE !layer-bottom depth of the 100-L soil layers to 6.0 m
! ----------------------------------------------------------------------

   WD1 = 0.
   DO K = 1,NSOIL
     WD1 = WD1 + (parameters%SMCMAX(1)-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   ! INitial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/parameters%PSISAT(1)
     WD2   = WD2 + parameters%SMCMAX(1)*(1.-TEMP**(-1./parameters%BEXP(1)))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ

!== begIN INfil ====================================================================================

  SUBROUTINE INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !IN
                    SICEMAX,QINSUR ,                         & !IN
                    PDDUM  ,RUNSRF )                           !OUT
! --------------------------------------------------------------------------------
! compute INflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! INputs
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
  REAL,                     INTENT(IN) :: DT     !time step (sec)
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
  REAL,                     INTENT(IN) :: QINSUR !water INput on soil surface [mm/s]
  REAL,                     INTENT(IN) :: SICEMAX!maximum soil ice content (m3/m3)

! OUTputs
  REAL,                    INTENT(OUT) :: RUNSRF !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM  !INfiltration rate at surface

! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum INfiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1. - EXP (-ACRT) * SUM
       END IF

! correction of INfiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL

!== begIN srt ======================================================================================

  SUBROUTINE SRT (parameters,NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & !IN
                  QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !IN
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !IN
                  RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !OUT
                  WCND   )                                   !OUT
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! water diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid INdex
    INTEGER,                  INTENT(IN)  :: JLOC   !grid INdex
    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ZSOIL
    REAL,                     INTENT(IN)  :: DT
    REAL,                     INTENT(IN)  :: PDDUM
    REAL,                     INTENT(IN)  :: QSEVA
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ETRANI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC
    REAL,                     INTENT(IN)  :: ZWT    ! water table depth [m]
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: FCR
    REAL, INTENT(IN)                      :: FCRMAX !maximum of FCR (-)
    REAL,                     INTENT(IN)  :: SICEMAX!maximum soil ice content (m3/m3)
    REAL,                     INTENT(IN)  :: SMCWTD !soil moisture between bottom of the soil and the water table

! OUTput

    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: WCND    !hydraulic conductivity (m/s)
    REAL,                     INTENT(OUT) :: QDRAIN  !bottom draINage (m/s)

! local
    INTEGER                               :: K
    REAL, DIMENSION(1:NSOIL)              :: DDZ
    REAL, DIMENSION(1:NSOIL)              :: DENOM
    REAL, DIMENSION(1:NSOIL)              :: DSMDZ
    REAL, DIMENSION(1:NSOIL)              :: WFLUX
    REAL, DIMENSION(1:NSOIL)              :: WDF
    REAL, DIMENSION(1:NSOIL)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD !soil moisture between bottom of the soil and water table
    REAL                                  :: SMXBOT  !soil moisture below bottom to calculate flux

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(OPT_INF == 1) THEN
      DO K = 1, NSOIL
        CALL WDFCND1 (parameters,WDF(K),WCND(K),SMC(K),FCR(K),K)
        SMX(K) = SMC(K)
      END DO
        IF(OPT_RUN == 5)SMXWTD=SMCWTD
    END IF

    IF(OPT_INF == 2) THEN
      DO K = 1, NSOIL
        CALL WDFCND2 (parameters,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as IN the bottom layer
    END IF

    DO K = 1, NSOIL
       IF(K == 1) THEN
          DENOM(K) = - ZSOIL (K)
          TEMP1    = - ZSOIL (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       ELSE IF (K < NSOIL) THEN
          DENOM(k) = (ZSOIL(K-1) - ZSOIL(K))
          TEMP1    = (ZSOIL(K-1) - ZSOIL(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       ELSE
          DENOM(K) = (ZSOIL(K-1) - ZSOIL(K))
          IF(OPT_RUN == 1 .or. OPT_RUN == 2) THEN
             QDRAIN   = 0.
          END IF
          IF(OPT_RUN == 3) THEN
             QDRAIN   = parameters%SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN
!gmm INterpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K) + ZSOIL(K) - ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K  ) * DSMDZ(K  ) + WCND(K  )
          END IF   
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-WCND(K-1)+ETRANI(K) + QDRAIN
       END IF
    END DO

    DO K = 1, NSOIL
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < NSOIL) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO

! ----------------------------------------------------------------------
  END SUBROUTINE SRT

!== begIN sstep ====================================================================================

  SUBROUTINE SSTEP (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !IN
                    SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !IN
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & !INOUT
                    RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !INOUT
                    WPLUS  )                                   !OUT

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!INput

  type (noahmp_parameters), INTENT(IN) :: parameters
    INTEGER,                         INTENT(IN) :: ILOC   !grid INdex
    INTEGER,                         INTENT(IN) :: JLOC   !grid INdex
    INTEGER,                         INTENT(IN) :: NSOIL  !
    INTEGER,                         INTENT(IN) :: NSNOW  !
    REAL, INTENT(IN)                            :: DT
    REAL, INTENT(IN)                            :: ZWT
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]

!INput and OUTput
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTT
    REAL                    , INTENT(INOUT) :: SMCWTD
    REAL                    , INTENT(INOUT) :: QDRAIN
    REAL                    , INTENT(INOUT) :: DEEPRECH

!OUTput
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS
! ----------------------------------------------------------------------
    WPLUS = 0.0

    DO K = 1,NSOIL
       RHSTT (K) =   RHSTT(K) * DT
       AI (K)    =      AI(K) * DT
       BI (K)    = 1. + BI(K) * DT
       CI (K)    =      CI(K) * DT
    END DO

! copy values for INput variables before callINg rosr12

    DO K = 1,NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO

! call ROSR12 to solve the tri-diagonal matrix

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    DO K = 1,NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    ENDDO

!  excessive water above saturation IN a layer is moved to
!  its unsaturated layer like IN a bucket

!gmmwith opt_run=5 there is soil moisture below nsoil, to the water table
  IF(OPT_RUN == 5) THEN

!update smcwtd

     IF(ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL))THEN
!accumulate qdraIN to update deep water table and soil moisture later
        DEEPRECH =  DEEPRECH + DT * QDRAIN
     ELSE
        SMCWTD = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
        WPLUS        = MAX((SMCWTD-parameters%SMCMAX(NSOIL)), 0.0) * DZSNSO(NSOIL)
        WMINUS       = MAX((1.E-4-SMCWTD), 0.0) * DZSNSO(NSOIL)

        SMCWTD = MAX( MIN(SMCWTD,parameters%SMCMAX(NSOIL)) , 1.E-4)
        SH2O(NSOIL)    = SH2O(NSOIL) + WPLUS/DZSNSO(NSOIL)

!reduce fluxes at the bottom boundaries accordINgly
        QDRAIN = QDRAIN - WPLUS/DT
        DEEPRECH = DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(1) - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1) 
    SH2O(1)      = MIN(EPORE,SH2O(1))

   IF(WPLUS > 0.0) THEN
    SH2O(2)      = SH2O(2) + WPLUS/DZSNSO(2)
    DO K = 2,NSOIL-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K+1)    = SH2O(K+1) + WPLUS/DZSNSO(K+1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(NSOIL) - SICE(NSOIL) ) )
    WPLUS        = MAX((SH2O(NSOIL)-EPORE), 0.0) * DZSNSO(NSOIL) 
    SH2O(NSOIL)  = MIN(EPORE,SH2O(NSOIL))
   END IF
   
    SMC = SH2O + SICE

  END SUBROUTINE SSTEP

!== begIN wdfcnd1 ==================================================================================

  SUBROUTINE WDFCND1 (parameters,WDF,WCND,SMC,FCR,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput 
  type (noahmp_parameters), INTENT(IN) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR
    INTEGER,INTENT(IN)  :: ISOIL

! OUTput
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1

!== begIN wdfcnd2 ==================================================================================

  SUBROUTINE WDFCND2 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! OUTput
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1,FACTR2
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = 0.05/parameters%SMCMAX(ISOIL)
    FACTR2 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(FACTR1,FACTR2)
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR2 ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2

!== begIN groundwater ==============================================================================

  SUBROUTINE GROUNDWATER(parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !IN
                         STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !IN
                         SH2O   ,ZWT    ,WA     ,WT     ,         & !INOUT
                         QIN    ,QDIS   )                           !OUT
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC  !grid INdex
  INTEGER,                         INTENT(IN) :: JLOC  !grid INdex
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL,                            INTENT(IN) :: DT    !timestep [sec]
  REAL,                            INTENT(IN) :: FCRMAX!maximum FCR (-)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: WCND  !hydraulic conductivity (m/s)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC   !snow/soil temperature (k)

! INput and OUTput
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O  !liquid soil water [m3/m3]
  REAL,                         INTENT(INOUT) :: ZWT   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: WA    !water storage IN aquifer [mm]
  REAL,                         INTENT(INOUT) :: WT    !water storage IN aquifer 
                                                           !+ saturated soil [mm]
! OUTput
  REAL,                           INTENT(OUT) :: QIN   !groundwater recharge [mm/s]
  REAL,                           INTENT(OUT) :: QDIS  !groundwater discharge [mm/s]

! local
  REAL                                        :: FFF   !runoff decay factor (m-1)
  REAL                                        :: RSBMX !baseflow coefficient [mm/s]
  INTEGER                                     :: IZ    !do-loop INdex
  INTEGER                                     :: IWT   !layer INdex above water table layer
  REAL,  DIMENSION(    1:NSOIL)               :: DZMM  !layer thickness [mm]
  REAL,  DIMENSION(    1:NSOIL)               :: ZNODE !node depth [m]
  REAL,  DIMENSION(    1:NSOIL)               :: MLIQ  !liquid water mass [kg/m2 or mm]
  REAL,  DIMENSION(    1:NSOIL)               :: EPORE !effective porosity [-]
  REAL,  DIMENSION(    1:NSOIL)               :: HK    !hydraulic conductivity [mm/s]
  REAL,  DIMENSION(    1:NSOIL)               :: SMC   !total soil water  content [m3/m3]
  REAL(KIND=8)                                :: S_NODE!degree of saturation of IWT layer
  REAL                                        :: DZSUM !cumulative depth above water table [m]
  REAL                                        :: SMPFZ !matric potential (frozen effects) [mm]
  REAL                                        :: KA    !aquifer hydraulic conductivity [mm/s]
  REAL                                        :: WH_ZWT!water head at water table [mm]
  REAL                                        :: WH    !water head at layer above ZWT [mm]
  REAL                                        :: WS    !water used to fill air pore [mm]
  REAL                                        :: WTSUB !sum of HK*DZMM
  REAL                                        :: WATMIN!mINimum soil vol soil moisture [m3/m3]
  REAL                                        :: XS    !excessive water above saturation [mm]
  REAL, PARAMETER                             :: ROUS = 0.2    !specific yield [-]
  REAL, PARAMETER                             :: CMIC = 0.20   !microprore content (0.0-1.0)
                                                               !0.0-close to free draINage
! -------------------------------------------------------------
      QDIS      = 0.0
      QIN       = 0.0

! Derive layer-bottom depth IN [mm]
!KWM:  Derive layer thickness IN mm

      DZMM(1) = -ZSOIL(1)*1.E3
      DO IZ = 2, NSOIL
         DZMM(IZ)  = 1.E3 * (ZSOIL(IZ - 1) - ZSOIL(IZ))
      ENDDO

! Derive node (middle) depth IN [m]
!KWM:  Positive number, depth below ground surface IN m
      ZNODE(1) = -ZSOIL(1) / 2.
      DO IZ = 2, NSOIL
         ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * (ZSOIL(IZ-1) - ZSOIL(IZ))
      ENDDO

! Convert volumetric soil moisture "sh2o" to mass

      DO IZ = 1, NSOIL
         SMC(IZ)      = SH2O(IZ) + SICE(IZ)
         MLIQ(IZ)     = SH2O(IZ) * DZMM(IZ)
         EPORE(IZ)    = MAX(0.01,parameters%SMCMAX(IZ) - SICE(IZ))
         HK(IZ)       = 1.E3*WCND(IZ)
      ENDDO

! The layer INdex of the first unsaturated layer,
! i.e., the layer right above the water table

      IWT = NSOIL
      DO IZ = 2,NSOIL
         IF(ZWT   .LE. -ZSOIL(IZ) ) THEN
            IWT = IZ-1
            EXIT
         END IF
      ENDDO

! Groundwater discharge [mm/s]

      FFF   = 6.0
      RSBMX = 5.0

      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-parameters%TIMEAN)*EXP(-FFF*(ZWT-2.0))

! Matric potential at the layer above the water table

      S_NODE = MIN(1.0,SMC(IWT)/parameters%SMCMAX(IWT) )
      S_NODE = MAX(S_NODE,REAL(0.01,KIND=8))
      SMPFZ  = -parameters%PSISAT(IWT)*1000.*S_NODE**(-parameters%BEXP(IWT))   ! m --> mm
      SMPFZ  = MAX(-120000.0,CMIC*SMPFZ)   

! Recharge rate qIN to groundwater

      KA  = HK(IWT)

      WH_ZWT  = - ZWT * 1.E3                          !(mm)
      WH      = SMPFZ  - ZNODE(IWT)*1.E3              !(mm)
      QIN     = - KA * (WH_ZWT-WH)  /((ZWT-ZNODE(IWT))*1.E3)
      QIN     = MAX(-10.0/DT,MIN(10./DT,QIN))
     
! Water storage IN the aquifer + saturated soil

      WT  = WT + (QIN - QDIS) * DT     !(mm)

      IF(IWT.EQ.NSOIL) THEN
         WA          = WA + (QIN - QDIS) * DT     !(mm)
         WT          = WA
         ZWT         = (-ZSOIL(NSOIL) + 25.) - WA/1000./ROUS      !(m)
         MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        ! [mm]

         MLIQ(NSOIL) = MLIQ(NSOIL) + MAX(0.,(WA - 5000.))
         WA          = MIN(WA, 5000.)
      ELSE
         
         IF (IWT.EQ.NSOIL-1) THEN
            ZWT = -ZSOIL(NSOIL)                   &
                 - (WT-ROUS*1000*25.) / (EPORE(NSOIL))/1000.
         ELSE
            WS = 0.   ! water used to fill soil air pores
            DO IZ = IWT+2,NSOIL
               WS = WS + EPORE(IZ) * DZMM(IZ)
            ENDDO
            ZWT = -ZSOIL(IWT+1)                  &
                  - (WT-ROUS*1000.*25.-WS) /(EPORE(IWT+1))/1000.
         ENDIF

         WTSUB = 0.
         DO IZ = 1, NSOIL
           WTSUB = WTSUB + HK(IZ)*DZMM(IZ)
         END DO

         DO IZ = 1, NSOIL           ! RemovINg subsurface runoff
         MLIQ(IZ) = MLIQ(IZ) - QDIS*DT*HK(IZ)*DZMM(IZ)/WTSUB
         END DO
      END IF

      ZWT = MAX(1.5,ZWT)

!
! Limit MLIQ to be greater than or equal to watmIN.
! Get water needed to brINg MLIQ equal WATMIN from lower layer.
!
      WATMIN = 0.01
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        WA       = WA - XS
        WT       = WT - XS

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / DZMM(IZ)
      END DO

  END SUBROUTINE GROUNDWATER

!== begIN shallowwatertable ========================================================================

  SUBROUTINE SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL  ,ZSOIL, DT    , & !IN
                         DZSNSO ,SMCEQ ,ILOC   ,JLOC         , & !IN
                         SMC    ,WTD   ,SMCWTD ,RECH, QDRAIN  )  !INOUT
! ----------------------------------------------------------------------
!Diagnoses water table depth and computes recharge when the water table is withIN the resolved soil layers,
!accordINg to the Miguez-Macho&Fan scheme
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! INput
  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  INTEGER,                         INTENT(IN) :: ILOC,JLOC
  REAL,                            INTENT(IN) :: DT
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]
  REAL,  DIMENSION(      1:NSOIL), INTENT(IN) :: SMCEQ  !equilibrium soil water  content [m3/m3]

! INput and OUTput
  REAL,  DIMENSION(      1:NSOIL), INTENT(INOUT) :: SMC   !total soil water  content [m3/m3]
  REAL,                         INTENT(INOUT) :: WTD   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: SMCWTD   !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL,                         INTENT(OUT) :: RECH ! groundwater recharge (net vertical flux across the water table), positive up
  REAL,                         INTENT(INOUT) :: QDRAIN
    
! local
  INTEGER                                     :: IZ    !do-loop INdex
  INTEGER                                     :: IWTD   !layer INdex above water table layer
  INTEGER                                     :: KWTD   !layer INdex where the water table layer is
  REAL                                        :: WTDOLD
  REAL                                        :: DZUP
  REAL                                        :: SMCEQDEEP
  REAL,  DIMENSION(       0:NSOIL)            :: ZSOIL0
! -------------------------------------------------------------


ZSOIL0(1:NSOIL) = ZSOIL(1:NSOIL)
ZSOIL0(0) = 0.         
 
!fINd the layer where the water table is
     DO IZ=NSOIL,1,-1
        IF(WTD + 1.E-6 < ZSOIL0(IZ)) EXIT
     ENDDO
        IWTD=IZ

        
        KWTD=IWTD+1  !layer where the water table is
        IF(KWTD.LE.NSOIL)THEN    !wtd IN the resolved layers
           WTDOLD=WTD
           IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
        
               IF(SMC(KWTD).EQ.parameters%SMCMAX(KWTD))THEN !wtd went to the layer above
                      WTD=ZSOIL0(IWTD)
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      IWTD=IWTD-1
                      KWTD=KWTD-1
                   IF(KWTD.GE.1)THEN
                      IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                      WTDOLD=WTD
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=RECH-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      ENDIF
                   ENDIF
               ELSE  !wtd stays IN the layer
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               ENDIF
           
           ELSE    !wtd has gone down to the layer below
               WTD=ZSOIL0(KWTD)
               RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               KWTD=KWTD+1
               IWTD=IWTD+1
!wtd crossed to the layer below. Now adjust it there
               IF(KWTD.LE.NSOIL)THEN
                   WTDOLD=WTD
                   IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                   WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                   - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                       ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ) , ZSOIL0(IWTD) )
                   ELSE
                   WTD=ZSOIL0(KWTD)
                   ENDIF
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))

                ELSE
                   WTDOLD=WTD
!restore smoi to equilibrium value with water from the ficticious layer below
!                   SMCWTD=SMCWTD-(SMCEQ(NSOIL)-SMC(NSOIL))
!                   QDRAIN = QDRAIN - 1000 * (SMCEQ(NSOIL)-SMC(NSOIL)) * DZSNSO(NSOIL) / DT
!                   SMC(NSOIL)=SMCEQ(NSOIL)
!adjust wtd IN the ficticious layer below
                   SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1./parameters%BEXP(NSOIL))
                   WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                   - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                       ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
                ENDIF
            
            ENDIF
        ELSEIF(WTD.GE.ZSOIL0(NSOIL)-DZSNSO(NSOIL))THEN
!if wtd was already below the bottom of the resolved soil crust
           WTDOLD=WTD
           SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1./parameters%BEXP(NSOIL))
           IF(SMCWTD.GT.SMCEQDEEP)THEN
               WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                 - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                     ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
               RECH = -(WTDOLD-WTD) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
           ELSE
               RECH = -(WTDOLD-(ZSOIL0(NSOIL)-DZSNSO(NSOIL))) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTDOLD=ZSOIL0(NSOIL)-DZSNSO(NSOIL)
!and now even further down
               DZUP=(SMCEQDEEP-SMCWTD)*DZSNSO(NSOIL)/(parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTD=WTDOLD-DZUP
               RECH = RECH - (parameters%SMCMAX(NSOIL)-SMCEQDEEP)*DZUP
               SMCWTD=SMCEQDEEP
           ENDIF

         
         ENDIF

IF(IWTD.LT.NSOIL .AND. IWTD.GT.0) THEN
  SMCWTD=parameters%SMCMAX(IWTD)
ELSEIF(IWTD.LT.NSOIL .AND. IWTD.LE.0) THEN
  SMCWTD=parameters%SMCMAX(1)
END IF

END  SUBROUTINE SHALLOWWATERTABLE

! ==================================================================================================
! ********************* end of water SUBROUTINEs ******************************************
! ==================================================================================================

!== begIN carbon ===================================================================================

  SUBROUTINE CARBON (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !IN
                     DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !IN
                     FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !IN
                     TROOT  ,IST    ,LAT    ,ILOC   ,JLOC   , & !IN
                     LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !INOUT
                     GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !OUT
                     TOTLB  ,XLAI   ,XSAI   )                   !OUT
! ------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ------------------------------------------------------------------------------------------
! INputs (carbon)

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN) :: ILOC   !grid INdex
  INTEGER                        , INTENT(IN) :: JLOC   !grid INdex
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottom from soil surface
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL                           , INTENT(IN) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(IN) :: TG     !ground temperature (k)
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL                           , INTENT(IN) :: PSN    !total leaf photosyn (umolco2/m2/s) [+]
  REAL                           , INTENT(IN) :: APAR   !PAR by canopy (w/m2)
  REAL                           , INTENT(IN) :: IGS    !growINg season INdex (0=off, 1=on)
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake

! INput & OUTput (carbon)

  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fINe roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (INcl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon IN deep soil [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short-lived carbon IN shallow soil [g/m2]

! OUTputs: (carbon)

  REAL                          , INTENT(OUT) :: GPP    !net INstantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2/s C]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange [g/m2/s CO2]
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: HETERS !organic respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon [g/m2 C]
  REAL                          , INTENT(OUT) :: TOTLB  !total livINg carbon ([g/m2 C]
  REAL                          , INTENT(OUT) :: XLAI   !leaf area INdex [-]
  REAL                          , INTENT(OUT) :: XSAI   !stem area INdex [-]
!  REAL                          , INTENT(OUT) :: VOCFLX(5) ! voc fluxes [ug C m-2 h-1]

! local variables

  INTEGER :: J         !do-loop INdex
  REAL    :: WROOT     !root zone soil water [-]
  REAL    :: WSTRES    !water stress coeficient [-]  (1. for wiltINg )
  REAL    :: LAPM      !leaf area per unit mass [m2/g]
! ------------------------------------------------------------------------------------------

   IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
        ( VEGTYP == parameters%ISICE ) .or. (parameters%urban_flag) ) THEN
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.

      RETURN
   END IF

      LAPM       = parameters%SLA / 1000.   ! m2/kg -> m2/g

! water stress

      WSTRES  = 1.- BTRAN

      WROOT  = 0.
      DO J=1,parameters%NROOT
        WROOT = WROOT + SMC(J)/parameters%SMCMAX(J) *  DZSNSO(J) / (-ZSOIL(parameters%NROOT))
      ENDDO

  CALL CO2FLUX (parameters,NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !IN
                DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !IN
                WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & !IN
                LAT    ,ILOC   ,JLOC   ,FVEG   ,         & !IN
                XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & !INOUT
                FASTCP ,STBLCP ,WOOD   ,                 & !INOUT
                GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !OUT
                TOTSC  ,TOTLB  )                           !OUT

!   CALL BVOC (parameters,VOCFLX,  VEGTYP,  VEGFAC,   APAR,   TV)
!   CALL CH4

  END SUBROUTINE CARBON

!== begIN co2flux ==================================================================================

  SUBROUTINE CO2FLUX (parameters,NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !IN
                      DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !IN
                      WROOT  ,WSTRES ,FOLN   ,LAPM   ,         & !IN
                      LAT    ,ILOC   ,JLOC   ,FVEG   ,         & !IN
                      XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS , & !INOUT
                      FASTCP ,STBLCP ,WOOD   ,                 & !INOUT
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !OUT
                      TOTSC  ,TOTLB  )                           !OUT
! -----------------------------------------------------------------------------------------
! The origINal code is from RE DickINson et al.(1998), modifed by Guo-Yue Niu, 2004
! -----------------------------------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------------------------------

! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN) :: ILOC   !grid INdex
  INTEGER                        , INTENT(IN) :: JLOC   !grid INdex
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation physiology type
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: IGS    !growINg season INdex (0=off, 1=on)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL                           , INTENT(IN) :: PSN    !total leaf photosynthesis (umolco2/m2/s)
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  REAL                           , INTENT(IN) :: TV     !leaf temperature (k)
  REAL                           , INTENT(IN) :: WROOT  !root zone soil water
  REAL                           , INTENT(IN) :: WSTRES !soil water stress
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: LAPM   !leaf area per unit mass [m2/g]
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction

! INput and OUTput

  REAL                        , INTENT(INOUT) :: XLAI   !leaf  area INdex from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area INdex from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fINe roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short lived carbon [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon pool [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (INcl. woody roots) [g/m2]

! OUTput

  REAL                          , INTENT(OUT) :: GPP    !net INstantaneous assimilation [g/m2/s]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange (autors+heters-gpp)
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem resp. (maINtance and growth)
  REAL                          , INTENT(OUT) :: HETERS !organic respiration
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon (g/m2)
  REAL                          , INTENT(OUT) :: TOTLB  !total livINg carbon (g/m2)

! local

  REAL                   :: CFLUX    !carbon flux to atmosphere [g/m2/s]
  REAL                   :: LFMSMN   !mINimum leaf mass [g/m2]
  REAL                   :: RSWOOD   !wood respiration [g/m2]
  REAL                   :: RSLEAF   !leaf maINtenance respiration per timestep [g/m2]
  REAL                   :: RSROOT   !fINe root respiration per time step [g/m2]
  REAL                   :: NPPL     !leaf net primary productivity [g/m2/s]
  REAL                   :: NPPR     !root net primary productivity [g/m2/s]
  REAL                   :: NPPW     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPS     !wood net primary productivity [g/m2/s]
  REAL                   :: DIELF    !death of leaf mass per time step [g/m2]

  REAL                   :: ADDNPPLF !leaf assimil after resp. losses removed [g/m2]
  REAL                   :: ADDNPPST !stem assimil after resp. losses removed [g/m2]
  REAL                   :: CARBFX   !carbon assimilated per model step [g/m2]
  REAL                   :: GRLEAF   !growth respiration rate for leaf [g/m2/s]
  REAL                   :: GRROOT   !growth respiration rate for root [g/m2/s]
  REAL                   :: GRWOOD   !growth respiration rate for wood [g/m2/s]
  REAL                   :: GRSTEM   !growth respiration rate for stem [g/m2/s]
  REAL                   :: LEAFPT   !fraction of carbon allocated to leaves [-]
  REAL                   :: LFDEL    !maximum  leaf mass  available to change [g/m2/s]
  REAL                   :: LFTOVR   !stem turnover per time step [g/m2]
  REAL                   :: STTOVR   !stem turnover per time step [g/m2]
  REAL                   :: WDTOVR   !wood turnover per time step [g/m2]
  REAL                   :: RSSOIL   !soil respiration per time step [g/m2]
  REAL                   :: RTTOVR   !root carbon loss per time step by turnover [g/m2]
  REAL                   :: STABLC   !decay rate of fast carbon to slow carbon [g/m2/s]
  REAL                   :: WOODF    !calculated wood to root ratio [-]
  REAL                   :: NONLEF   !fraction of carbon to root and wood [-]
  REAL                   :: ROOTPT   !fraction of carbon flux to roots [-]
  REAL                   :: WOODPT   !fraction of carbon flux to wood [-]
  REAL                   :: STEMPT   !fraction of carbon flux to stem [-]
  REAL                   :: RESP     !leaf respiration [umol/m2/s]
  REAL                   :: RSSTEM   !stem respiration [g/m2/s]

  REAL                   :: FSW      !soil water factor for microbial respiration
  REAL                   :: FST      !soil temperature factor for microbial respiration
  REAL                   :: FNF      !foliage nitrogen adjustemt to respiration (<= 1)
  REAL                   :: TF       !temperature factor
  REAL                   :: RF       !respiration reduction factor (<= 1)
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     !stem area per unit mass (m2/g)
  REAL                   :: DIEST
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
  REAL                   :: RSDRYC   !degree of dryINg that reduces soil respiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !mINimum leaf area INdex [m2/m2]
  REAL                   :: XSAMIN   !mINimum leaf area INdex [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC

! Respiration as a function of temperature

  REAL :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

! constants
    RTOVRC  = 2.0E-8        !origINal was 2.0e-8
    RSDRYC  = 40.0          !origINal was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !origINal was 0.90   ! carbon to roots
    WSTRC   = 100.0
    LAIMIN  = 0.05   
    XSAMIN  = 0.05     ! MB: change to prevent vegetation from not growINg back IN sprINg

    SAPM    = 3.*0.001      ! m2/kg -->m2/g
    LFMSMN  = laimIN/lapm
    STMSMN  = xsamIN/sapm
! ---------------------------------------------------------------------------------

! respiration

     IF(IGS .EQ. 0.) THEN
       RF = 0.5
     ELSE
       RF = 1.0
     ENDIF
            
     FNF     = MIN( FOLN/MAX(1.E-06,parameters%FOLNMX), 1.0 )
     TF      = parameters%ARM**( (TV-298.16)/10. )
     RESP    = parameters%RMF25 * TF * FNF * XLAI * RF * (1.-WSTRES) ! umol/m2/s
     RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*12.e-6)                         ! g/m2/s
     
     RSROOT  = parameters%RMR25*(RTMASS*1E-3)*TF *RF* 12.e-6         ! g/m2/s
     RSSTEM  = parameters%RMS25*((STMASS-STMSMN)*1E-3)*TF *RF* 12.e-6         ! g/m2/s
     RSWOOD  = RSWOODC * R(TV) * WOOD*parameters%WDPOOL

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;

     CARBFX  = PSN * 12.e-6              ! umol co2 /m2/ s -> g/m2/s carbon

! fraction of carbon INto leaf versus nonleaf

     LEAFPT = EXP(0.01*(1.-EXP(0.75*XLAI))*XLAI)
     IF(VEGTYP == parameters%EBLFOREST) LEAFPT = EXP(0.01*(1.-EXP(0.50*XLAI))*XLAI)

     NONLEF = 1.0 - LEAFPT
     STEMPT = XLAI/10.0*LEAFPT
     LEAFPT = LEAFPT - STEMPT

!  fraction of carbon INto wood versus root

     IF(WOOD > 1.e-6) THEN
        WOODF = (1.-EXP(-BF*(parameters%WRRAT*RTMASS/WOOD))/BF)*parameters%WDPOOL
     ELSE
        WOODF = parameters%WDPOOL
     ENDIF

     ROOTPT = NONLEF*(1.-WOODF)
     WOODPT = NONLEF*WOODF

! leaf and root turnover per time step

     LFTOVR = parameters%LTOVRC*5.E-7*LFMASS
     STTOVR = parameters%LTOVRC*5.E-7*STMASS
     RTTOVR = RTOVRC*RTMASS
     WDTOVR = 9.5E-10*WOOD

! seasonal leaf die rate dependent on temp and water stress
! water stress is set to 1 at permanent wiltINg poINt

     SC  = EXP(-0.3*MAX(0.,TV-parameters%TDLEF)) * (LFMASS/120.) 
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(parameters%DILEFW * SD + parameters%DILEFC*SC)
     DIEST = STMASS*1.E-6*(parameters%DILEFW * SD + parameters%DILEFC*SC)

! calculate growth respiration for leaf, rtmass and wood

     GRLEAF = MAX(0.0,parameters%FRAGR*(LEAFPT*CARBFX - RSLEAF))
     GRSTEM = MAX(0.0,parameters%FRAGR*(STEMPT*CARBFX - RSSTEM))
     GRROOT = MAX(0.0,parameters%FRAGR*(ROOTPT*CARBFX - RSROOT))
     GRWOOD = MAX(0.0,parameters%FRAGR*(WOODPT*CARBFX - RSWOOD))

! Impose lower T limit for photosynthesis

     ADDNPPLF = MAX(0.,LEAFPT*CARBFX - GRLEAF-RSLEAF)
     ADDNPPST = MAX(0.,STEMPT*CARBFX - GRSTEM-RSSTEM)
!     ADDNPPLF = LEAFPT*CARBFX - GRLEAF-RSLEAF  ! MB: test Kjetil 
!     ADDNPPST = STEMPT*CARBFX - GRSTEM-RSSTEM  ! MB: test Kjetil 
     IF(TV.LT.parameters%TMIN) ADDNPPLF =0.
     IF(TV.LT.parameters%TMIN) ADDNPPST =0.

! update leaf, root, and wood carbon
! avoid reducINg leaf mass below its mINimum value but conserve mass

     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)
     DIEST = MIN(DIEST,STDEL+ADDNPPST-STTOVR)

! net primary productivities

     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPR   = ROOTPT*CARBFX - RSROOT - GRROOT
     NPPW   = WOODPT*CARBFX - RSWOOD - GRWOOD

! masses of plant components

     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR-DIEST)*DT   ! g/m2
     RTMASS = RTMASS + (NPPR-RTTOVR)      *DT

     IF(RTMASS.LT.0.0) THEN
           RTTOVR = NPPR
           RTMASS = 0.0
     ENDIF
     WOOD = (WOOD+(NPPW-WDTOVR)*DT)*parameters%WDPOOL

! soil carbon budgets

     FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF+DIEST)*DT  ! MB: add DIEST v3.7

     FST = 2.0**( (STC(1)-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * parameters%MRP* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT

!  total carbon flux

     CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &  ! MB: add RSSTEM,GRSTEM,0.9*RSSOIL v3.7
          + 0.9*RSSOIL + GRLEAF + GRROOT + GRWOOD + GRSTEM    ! g/m2/s

! for OUTputs

     GPP    = CARBFX                                             !g/m2/s C
     NPP    = NPPL + NPPW + NPPR +NPPS                           !g/m2/s C
     AUTORS = RSROOT + RSWOOD  + RSLEAF + RSSTEM + &             !g/m2/s C  MB: add RSSTEM, GRSTEM v3.7
              GRLEAF + GRROOT + GRWOOD + GRSTEM                  !g/m2/s C  MB: add 0.9* v3.7
     HETERS = 0.9*RSSOIL                                         !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)*44./12.                    !g/m2/s CO2
     TOTSC  = FASTCP + STBLCP                                    !g/m2   C
     TOTLB  = LFMASS + RTMASS +STMASS + WOOD                     !g/m2   C  MB: add STMASS v3.7

! leaf area INdex and stem area INdex

     XLAI    = MAX(LFMASS*LAPM,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)
    
  END SUBROUTINE CO2FLUX

!== begIN carbon_crop ==============================================================================

 SUBROUTINE CARBON_CROP (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  ,JULIAN , & !IN
                            DZSNSO ,STC    ,SMC    ,TV     ,PSN    ,FOLN   ,BTRAN  , & !IN
                            SOLDN  ,T2M    ,                                         & !IN
                            LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP ,GRAIN  , & !INOUT
			    XLAI   ,XSAI   ,GDD    ,                                 & !INOUT
                            GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  ,TOTLB, PGS    ) !OUT
! ------------------------------------------------------------------------------------------
! Initial crop version created by XINg Liu
! Initial crop version added by Barlage v3.8

! ------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ------------------------------------------------------------------------------------------
! INputs (carbon)

  type (noahmp_parameters), INTENT(IN) :: parameters
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation type 
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottomfrom soil surface
  REAL                           , INTENT(IN) :: JULIAN !Julian day of year(fractional) ( 0 <= JULIAN < YEARLEN )
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layerthickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature[k]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice +liq.) [m3/m3]
  REAL                           , INTENT(IN) :: TV     !vegetation temperature(k)
  REAL                           , INTENT(IN) :: PSN    !total leaf photosyn(umolco2/m2/s) [+]
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: BTRAN  !soil watertranspiration factor (0 to 1)
  REAL                           , INTENT(IN) :: SOLDN  !Downward solar radiation
  REAL                           , INTENT(IN) :: T2M    !air temperature

! INput & OUTput (carbon)

  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fINe roots[g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (INcl.woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon IN deepsoil [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short-lived carbon INshallow soil [g/m2]
  REAL                        , INTENT(INOUT) :: GRAIN  !mass of GRAIN [g/m2]
  REAL                        , INTENT(INOUT) :: XLAI   !leaf area INdex [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area INdex [-]
  REAL                        , INTENT(INOUT) :: GDD    !growINg degree days

! OUTOUT
  REAL                          , INTENT(OUT) :: GPP    !net INstantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2/s C]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange[g/m2/s CO2]
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: HETERS !organic respiration[g/m2/s C]
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon [g/m2C]
  REAL                          , INTENT(OUT) :: TOTLB  !total livINg carbon ([g/m2 C]

! local variables

  INTEGER :: J         !do-loop INdex
  REAL    :: WROOT     !root zone soil water [-]
  REAL    :: WSTRES    !water stress coeficient [-]  (1. for wiltINg )
  INTEGER :: IPA       !PlantINg INdex
  INTEGER :: IHA       !HavestINdex(0=on,1=off)
  INTEGER, INTENT(OUT) :: PGS       !Plant growth stage

  REAL    :: PSNCROP 

! ------------------------------------------------------------------------------------------
   IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
        ( VEGTYP == parameters%ISICE ) .or. (parameters%urban_flag) ) THEN
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.
      GRAIN  = 0.
      RETURN
   END IF

! water stress


   WSTRES  = 1.- BTRAN

   WROOT  = 0.
   DO J=1,parameters%NROOT
     WROOT = WROOT + SMC(J)/parameters%SMCMAX(J) *  DZSNSO(J) / (-ZSOIL(parameters%NROOT))
   ENDDO

   CALL PSN_CROP     ( parameters,                           & !IN
                       SOLDN,   XLAI,    T2M,                & !IN 
                       PSNCROP                             )   !OUT

   CALL GROWING_GDD  (parameters,                           & !IN
                      T2M ,   DT,  JULIAN,                  & !IN
                      GDD ,                                 & !INOUT 
                      IPA ,  IHA,     PGS)                    !OUT                        

   CALL CO2FLUX_CROP (parameters,                              & !IN
                      DT     ,STC(1) ,PSN    ,TV     ,WROOT  ,WSTRES ,FOLN   , & !IN
                      IPA    ,IHA    ,PGS    ,                                 & !IN XING
                      XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS ,                 & !INOUT
                      FASTCP ,STBLCP ,WOOD   ,GRAIN  ,GDD    ,                 & !INOUT
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,                 & !OUT
                      TOTSC  ,TOTLB  )                                           !OUT

  END SUBROUTINE CARBON_CROP

!== begIN co2flux_crop =============================================================================

  SUBROUTINE CO2FLUX_CROP (parameters,                                              & !IN
                           DT     ,STC    ,PSN    ,TV     ,WROOT  ,WSTRES ,FOLN   , & !IN
                           IPA    ,IHA    ,PGS    ,                                 & !IN XING
                           XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS ,                 & !INOUT
                           FASTCP ,STBLCP ,WOOD   ,GRAIN  ,GDD,                     & !INOUT
                           GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,                 & !OUT
                           TOTSC  ,TOTLB  )                                           !OUT
! -----------------------------------------------------------------------------------------
! The origINal code from RE DickINson et al.(1998) and Guo-Yue Niu(2004),
! modified by XINg Liu, 2014.
! 
! -----------------------------------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------------------------------

! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL                           , INTENT(IN) :: STC    !soil temperature[k]
  REAL                           , INTENT(IN) :: PSN    !total leaf photosynthesis (umolco2/m2/s)
  REAL                           , INTENT(IN) :: TV     !leaf temperature (k)
  REAL                           , INTENT(IN) :: WROOT  !root zone soil water
  REAL                           , INTENT(IN) :: WSTRES !soil water stress
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  INTEGER                        , INTENT(IN) :: IPA
  INTEGER                        , INTENT(IN) :: IHA
  INTEGER                        , INTENT(IN) :: PGS

! INput and OUTput

  REAL                        , INTENT(INOUT) :: XLAI   !leaf  area INdex from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area INdex from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fINe roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short lived carbon [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon pool [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (INcl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: GRAIN  !mass of graIN (XING) [g/m2]
  REAL                        , INTENT(INOUT) :: GDD    !growINg degree days (XING)

! OUTput

  REAL                          , INTENT(OUT) :: GPP    !net INstantaneous assimilation [g/m2/s]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange (autors+heters-gpp)
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem resp. (maINtance and growth)
  REAL                          , INTENT(OUT) :: HETERS !organic respiration
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon (g/m2)
  REAL                          , INTENT(OUT) :: TOTLB  !total livINg carbon (g/m2)

! local

  REAL                   :: CFLUX    !carbon flux to atmosphere [g/m2/s]
  REAL                   :: LFMSMN   !mINimum leaf mass [g/m2]
  REAL                   :: RSWOOD   !wood respiration [g/m2]
  REAL                   :: RSLEAF   !leaf maINtenance respiration per timestep[g/m2]
  REAL                   :: RSROOT   !fINe root respiration per time step [g/m2]
  REAL                   :: RSGRAIN  !graIN respiration [g/m2]  
  REAL                   :: NPPL     !leaf net primary productivity [g/m2/s]
  REAL                   :: NPPR     !root net primary productivity [g/m2/s]
  REAL                   :: NPPW     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPS     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPG     !graIN net primary productivity [g/m2/s] 
  REAL                   :: DIELF    !death of leaf mass per time step [g/m2]

  REAL                   :: ADDNPPLF !leaf assimil after resp. losses removed[g/m2]
  REAL                   :: ADDNPPST !stem assimil after resp. losses removed[g/m2]
  REAL                   :: CARBFX   !carbon assimilated per model step [g/m2]
  REAL                   :: CBHYDRAFX!carbonhydrate assimilated per model step [g/m2]
  REAL                   :: GRLEAF   !growth respiration rate for leaf [g/m2/s]
  REAL                   :: GRROOT   !growth respiration rate for root [g/m2/s]
  REAL                   :: GRWOOD   !growth respiration rate for wood [g/m2/s]
  REAL                   :: GRSTEM   !growth respiration rate for stem [g/m2/s]
  REAL                   :: GRGRAIN   !growth respiration rate for stem [g/m2/s]
  REAL                   :: LEAFPT   !fraction of carbon allocated to leaves [-]
  REAL                   :: LFDEL    !maximum  leaf mass  available to change[g/m2/s]
  REAL                   :: LFTOVR   !stem turnover per time step [g/m2]
  REAL                   :: STTOVR   !stem turnover per time step [g/m2]
  REAL                   :: WDTOVR   !wood turnover per time step [g/m2]
  REAL                   :: GRTOVR   !graINturnover per time step [g/m2]
  REAL                   :: RSSOIL   !soil respiration per time step [g/m2]
  REAL                   :: RTTOVR   !root carbon loss per time step by turnover[g/m2]
  REAL                   :: STABLC   !decay rate of fast carbon to slow carbon[g/m2/s]
  REAL                   :: WOODF    !calculated wood to root ratio [-]
  REAL                   :: NONLEF   !fraction of carbon to root and wood [-]
  REAL                   :: RESP     !leaf respiration [umol/m2/s]
  REAL                   :: RSSTEM   !stem respiration [g/m2/s]

  REAL                   :: FSW      !soil water factor for microbial respiration
  REAL                   :: FST      !soil temperature factor for microbialrespiration
  REAL                   :: FNF      !foliage nitrogen adjustemt to respiration(<= 1)
  REAL                   :: TF       !temperature factor
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     !stem area per unit mass (m2/g)
  REAL                   :: DIEST
  REAL                   :: STCONVERT   !stem to graIN conversion [g/m2/s]
  REAL                   :: RTCONVERT   !root to graIN conversion [g/m2/s]
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
  REAL                   :: RSDRYC   !degree of dryINg that reduces soilrespiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !mINimum leaf area INdex [m2/m2]
  REAL                   :: XSAMIN   !mINimum leaf area INdex [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC
  REAL                   :: TEMP

! Respiration as a function of temperature

  REAL :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

! constants
    RSDRYC  = 40.0          !origINal was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !origINal was 0.90   ! carbon to roots
    WSTRC   = 100.0
    LAIMIN  = 0.05
    XSAMIN  = 0.05

    SAPM    = 3.*0.001      ! m2/kg -->m2/g
    LFMSMN  = laimIN/0.035
    STMSMN  = xsamIN/sapm
! ---------------------------------------------------------------------------------

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20

     CARBFX     = PSN*12.e-6!*IPA   !umol co2 /m2/ s -> g/m2/s C
     CBHYDRAFX  = PSN*30.e-6!*IPA

! maINteINance respiration
     FNF     = MIN( FOLN/MAX(1.E-06,parameters%FOLN_MX), 1.0 )
     TF      = parameters%Q10MR**( (TV-298.16)/10. )
     RESP    = parameters%LFMR25 * TF * FNF * XLAI  * (1.-WSTRES)  ! umol/m2/s
     RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*30.e-6)                       ! g/m2/s
     RSROOT  = parameters%RTMR25*(RTMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSSTEM  = parameters%STMR25*(STMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSGRAIN = parameters%GRAINMR25*(GRAIN*1E-3)*TF * 30.e-6       ! g/m2/s

! calculate growth respiration for leaf, rtmass and graIN

     GRLEAF  = MAX(0.0,parameters%FRA_GR*(parameters%LFPT(PGS)*CBHYDRAFX  - RSLEAF))
     GRSTEM  = MAX(0.0,parameters%FRA_GR*(parameters%STPT(PGS)*CBHYDRAFX  - RSSTEM))
     GRROOT  = MAX(0.0,parameters%FRA_GR*(parameters%RTPT(PGS)*CBHYDRAFX  - RSROOT))
     GRGRAIN = MAX(0.0,parameters%FRA_GR*(parameters%GRAINPT(PGS)*CBHYDRAFX  - RSGRAIN))

! leaf turnover, stem turnover, root turnover and leaf death caused by soil
! water and soil temperature stress

     LFTOVR  = parameters%LF_OVRC(PGS)*1.E-6*LFMASS
     RTTOVR  = parameters%RT_OVRC(PGS)*1.E-6*RTMASS
     STTOVR  = parameters%ST_OVRC(PGS)*1.E-6*STMASS
     SC  = EXP(-0.3*MAX(0.,TV-parameters%LEFREEZ)) * (LFMASS/120.)
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(parameters%DILE_FW(PGS) * SD + parameters%DILE_FC(PGS)*SC)

! Allocation of CBHYDRAFX to leaf, stem, root and graIN at each growth stage


     ADDNPPLF    = MAX(0.,parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF)
     ADDNPPLF    = parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF
     ADDNPPST    = MAX(0.,parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM)
     ADDNPPST    = parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM
    

! avoid reducINg leaf mass below its mINimum value but conserve mass

     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     LFTOVR  = MIN(LFTOVR,LFDEL+ADDNPPLF)
     STTOVR  = MIN(STTOVR,STDEL+ADDNPPST)
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)

! net primary productivities

     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPL   = ADDNPPLF
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPS   = ADDNPPST
     NPPR   = parameters%RTPT(PGS)*CBHYDRAFX - RSROOT - GRROOT
     NPPG  =  parameters%GRAINPT(PGS)*CBHYDRAFX - RSGRAIN - GRGRAIN

! masses of plant components
  
     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR)*DT   ! g/m2
     RTMASS = RTMASS + (NPPR-RTTOVR)*DT
     GRAIN =  GRAIN + NPPG*DT 

     GPP = CBHYDRAFX* 0.4 !!g/m2/s C  0.4=12/30, CH20 to C

     STCONVERT = 0.0
     RTCONVERT = 0.0
     IF(PGS==6) THEN
       STCONVERT = STMASS*(0.00005*DT/3600.0)
       STMASS = STMASS - STCONVERT
       RTCONVERT = RTMASS*(0.0005*DT/3600.0)
       RTMASS = RTMASS - RTCONVERT
       GRAIN  = GRAIN + STCONVERT + RTCONVERT
     END IF
    
     IF(RTMASS.LT.0.0) THEN
       RTTOVR = NPPR
       RTMASS = 0.0
     ENDIF

     IF(GRAIN.LT.0.0) THEN
       GRAIN = 0.0
     ENDIF

 ! soil carbon budgets

!     IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
!       FASTCP=1000
!     ELSE
       FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+DIELF)*DT 
!     END IF
     FST = 2.0**( (STC-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * parameters%MRP* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT

!  total carbon flux

     CFLUX  = - CARBFX + RSLEAF + RSROOT  + RSSTEM &
              + RSSOIL + GRLEAF + GRROOT                  ! g/m2/s 0.4=12/30, CH20 to C

! for OUTputs
                                                                 !g/m2/s C

     NPP   = (NPPL + NPPS+ NPPR +NPPG)*0.4      !!g/m2/s C  0.4=12/30, CH20 to C
 
  
     AUTORS = RSROOT + RSGRAIN  + RSLEAF +  &                     !g/m2/s C
              GRLEAF + GRROOT + GRGRAIN                           !g/m2/s C

     HETERS = RSSOIL                                             !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)*44./30.                    !g/m2/s CO2
     TOTSC  = FASTCP + STBLCP                                    !g/m2   C

     TOTLB  = LFMASS + RTMASS + GRAIN         

! leaf area INdex and stem area INdex
  
     XLAI    = MAX(LFMASS*parameters%BIO2LAI,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)

   
!After harverstINg
!     IF(PGS == 8 ) THEN
!       LFMASS = 0.62
!       STMASS = 0
!       GRAIN  = 0
!     END IF

!    IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
    IF(PGS == 8 .and. (GRAIN > 0. .or. LFMASS > 0 .or. STMASS > 0 .or. RTMASS > 0)) THEN
     XLAI   = 0.05
     XSAI   = 0.05
     LFMASS = LFMSMN
     STMASS = STMSMN
     RTMASS = 0
     GRAIN  = 0
    END IF 
    
END SUBROUTINE CO2FLUX_CROP

!== begIN growINg_gdd ==============================================================================

  SUBROUTINE GROWING_GDD (parameters,                         & !IN
                          T2M ,   DT, JULIAN,                 & !IN
                          GDD ,                               & !INOUT 
                          IPA,   IHA,     PGS)                  !OUT  
!===================================================================================================

! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
   REAL                     , INTENT(IN)        :: T2M     !Air temperature
   REAL                     , INTENT(IN)        :: DT      !time step (s)
   REAL                     , INTENT(IN)        :: JULIAN  !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )

! INput and OUTput

   REAL                     , INTENT(INOUT)     :: GDD     !growINg degress days

! OUTput

   INTEGER                  , INTENT(OUT)       :: IPA     !PlantINg INdex INdex(0=off, 1=on)
   INTEGER                  , INTENT(OUT)       :: IHA     !HavestINdex(0=on,1=off) 
   INTEGER                  , INTENT(OUT)       :: PGS     !Plant growth stage(1=S1,2=S2,3=S3)

!local 

   REAL                                         :: GDDDAY    !gap bewtween GDD and GDD8
   REAL                                         :: DAYOFS2   !DAYS IN stage2
   REAL                                         :: TDIFF     !temperature difference for growINg degree days calculation
   REAL                                         :: TC

   TC = T2M - 273.15

!HavestINdex(0=on,1=off) 

   IPA = 1
   IHA = 1

!turn on/off the plantINg 
 
   IF(JULIAN < parameters%PLTDAY)  IPA = 0

!turn on/off the harvestINg
    IF(JULIAN >= parameters%HSDAY) IHA = 0
   
!Calculate the growINg degree days
   
    IF(TC <  parameters%GDDTBASE) THEN
      TDIFF = 0.0
    ELSEIF(TC >= parameters%GDDTCUT) THEN
      TDIFF = parameters%GDDTCUT - parameters%GDDTBASE
    ELSE
      TDIFF = TC - parameters%GDDTBASE
    END IF

    GDD     = (GDD + TDIFF * DT / 86400.0) * IPA * IHA

    GDDDAY  = GDD

   ! Decide corn growth stage, based on Hybrid-Maize 
   !   PGS = 1 : Before plantINg
   !   PGS = 2 : from tassel INitiation to silkINg
   !   PGS = 3 : from silkINg to effective graIN fillINg
   !   PGS = 4 : from effective graIN fillINg to pysiological maturity 
   !   PGS = 5 : GDDM=1389
   !   PGS = 6 :
   !   PGS = 7 :
   !   PGS = 8 :
   !  GDDM = 1389
   !  GDDM = 1555
   ! GDDSK = 0.41*GDDM +145.4+150 !from hybrid-maize 
   ! GDDS1 = ((GDDSK-96)/38.9-4)*21
   ! GDDS1 = 0.77*GDDSK
   ! GDDS3 = GDDSK+170
   ! GDDS3 = 170

   PGS = 1                         ! MB: set PGS = 1 (for INitialization durINg growINg season when no GDD)

   IF(GDDDAY > 0.0) PGS = 2

   IF(GDDDAY >= parameters%GDDS1)  PGS = 3

   IF(GDDDAY >= parameters%GDDS2)  PGS = 4 

   IF(GDDDAY >= parameters%GDDS3)  PGS = 5

   IF(GDDDAY >= parameters%GDDS4)  PGS = 6

   IF(GDDDAY >= parameters%GDDS5)  PGS = 7

   IF(JULIAN >= parameters%HSDAY)  PGS = 8
 
   IF(JULIAN <  parameters%PLTDAY) PGS = 1   

END SUBROUTINE GROWING_GDD

!== begIN psn_crop =================================================================================

SUBROUTINE PSN_CROP ( parameters,       & !IN
                      SOLDN, XLAI,T2M,  & !IN
                      PSNCROP        )    !OUT
!===================================================================================================

! INput

  type (noahmp_parameters), INTENT(IN) :: parameters
  REAL     , INTENT(IN)    :: SOLDN    ! downward solar radiation
  REAL     , INTENT(IN)    :: XLAI     ! LAI
  REAL     , INTENT(IN)    :: T2M      ! air temp
  REAL     , INTENT(OUT)   :: PSNCROP  !

!local

  REAL                     :: PAR      ! photosynthetically active radiation (w/m2) 1 W m-2 = 0.0864 MJ m-2 day-1
  REAL                     :: Amax     ! Maximum CO2 assimulation rate g/co2/s  
  REAL                     :: L1       ! Three Gaussian method
  REAL                     :: L2       ! Three Gaussian method
  REAL                     :: L3       ! Three Gaussian method
  REAL                     :: I1       ! Three Gaussian method
  REAL                     :: I2       ! Three Gaussian method
  REAL                     :: I3       ! Three Gaussian method
  REAL                     :: A1       ! Three Gaussian method
  REAL                     :: A2       ! Three Gaussian method
  REAL                     :: A3       ! Three Gaussian method
  REAL                     :: A        ! CO2 Assimulation 
  REAL                     :: TC

  TC = T2M - 273.15

  PAR = parameters%I2PAR * SOLDN * 0.0036  !w to MJ m-2

  IF(TC < parameters%TASSIM0) THEN
    Amax = 1E-10
  ELSEIF(TC >= parameters%TASSIM0 .and. TC < parameters%TASSIM1) THEN
    Amax = (TC - parameters%TASSIM0) * parameters%Aref / (parameters%TASSIM1 - parameters%TASSIM0)
  ELSEIF(TC >= parameters%TASSIM1 .and. TC < parameters%TASSIM2) THEN
    Amax = parameters%Aref
  ELSE
    Amax= parameters%Aref - 0.2 * (T2M - parameters%TASSIM2)
  ENDIF 
  
  Amax = max(amax,0.01)

  IF(XLAI <= 0.05) THEN
    L1 = 0.1127 * 0.05   !use INitial LAI(0.05), avoid error
    L2 = 0.5    * 0.05
    L3 = 0.8873 * 0.05
  ELSE
    L1 = 0.1127 * XLAI
    L2 = 0.5    * XLAI
    L3 = 0.8873 * XLAI
  END IF

  I1 = parameters%k * PAR * exp(-parameters%k * L1)
  I2 = parameters%k * PAR * exp(-parameters%k * L2)
  I3 = parameters%k * PAR * exp(-parameters%k * L3)

  I1 = max(I1,1E-10)
  I2 = max(I2,1E-10)
  I3 = max(I3,1E-10)

  A1 = Amax * (1 - exp(-parameters%epsi * I1 / Amax))
  A2 = Amax * (1 - exp(-parameters%epsi * I2 / Amax)) * 1.6
  A3 = Amax * (1 - exp(-parameters%epsi * I3 / Amax))

  IF (XLAI <= 0.05) THEN
    A  = (A1+A2+A3) / 3.6 * 0.05
  ELSEIF (XLAI > 0.05 .and. XLAI <= 4.0) THEN
    A  = (A1+A2+A3) / 3.6 * XLAI
  ELSE
    A = (A1+A2+A3) / 3.6 * 4
  END IF

  A = A * parameters%PSNRF ! AttaINable 

  PSNCROP = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

END SUBROUTINE PSN_CROP

!== begIN bvocflux =================================================================================

!  SUBROUTINE BVOCFLUX(parameters,VOCFLX,  VEGTYP,  VEGFRAC,  APAR,   TV )
!
! ------------------------------------------------------------------------------------------
!      implicit none
! ------------------------------------------------------------------------------------------
!
! ------------------------ code history ---------------------------
! source file:       BVOC
! purpose:           BVOC emissions
! DESCRIPTION:
! Volatile organic compound emission 
! This code simulates volatile organic compound emissions
! followINg the algorithm presented IN Guenther, A., 1999: ModelINg
! Biogenic Volatile Organic Compound Emissions to the Atmosphere. In
! Reactive Hydrocarbons IN the Atmosphere, Ch. 3
! This model relies on the assumption that 90% of isoprene and monoterpene
! emissions origINate from canopy foliage:
!    E = epsilon * gamma * density * delta
! The factor delta (longterm activity factor) applies to isoprene emission
! from deciduous plants only. We neglect this factor at the present time.
! This factor is discussed IN Guenther (1997).
! SubrOUTINe written to operate at the patch level.
! IN FINAL IMPLEMENTATION, REMEMBER:
! 1. may wish to call this rOUTINe only as freq. as rad. calculations
! 2. may wish to place epsilon values directly IN pft-physiology file
! ------------------------ INput/OUTput variables -----------------
! INput
!  INTEGER                     ,INTENT(IN) :: vegtyp  !vegetation type 
!  REAL                        ,INTENT(IN) :: vegfrac !green vegetation fraction [0.0-1.0]
!  REAL                        ,INTENT(IN) :: apar    !photosynthesis active energy by canopy (w/m2)
!  REAL                        ,INTENT(IN) :: tv      !vegetation canopy temperature (k)
!
! OUTput
!  REAL                        ,INTENT(OUT) :: vocflx(5) ! voc fluxes [ug C m-2 h-1]
!
! Local Variables
!
!  REAL, parameter :: R      = 8.314    ! univ. gas constant [J K-1 mol-1]
!  REAL, parameter :: alpha  = 0.0027   ! empirical coefficient
!  REAL, parameter :: cl1    = 1.066    ! empirical coefficient
!  REAL, parameter :: ct1    = 95000.0  ! empirical coefficient [J mol-1]
!  REAL, parameter :: ct2    = 230000.0 ! empirical coefficient [J mol-1]
!  REAL, parameter :: ct3    = 0.961    ! empirical coefficient
!  REAL, parameter :: tm     = 314.0    ! empirical coefficient [K]
!  REAL, parameter :: tstd   = 303.0    ! std temperature [K]
!  REAL, parameter :: bet    = 0.09     ! beta empirical coefficient [K-1]
!
!  INTEGER ivoc        ! do-loop INdex
!  INTEGER ityp        ! do-loop INdex
!  REAL epsilon(5)
!  REAL gamma(5)
!  REAL density
!  REAL elai
!  REAL par,cl,reciprod,ct
!
! epsilon :
!
!    do ivoc = 1, 5
!    epsilon(ivoc) = parameters%eps(VEGTYP,ivoc)
!    end do
!
! gamma : Activity factor. Units [dimensionless]
!
!      reciprod = 1. / (R * tv * tstd)
!      ct = exp(ct1 * (tv - tstd) * reciprod) / &
!           (ct3 + exp(ct2 * (tv - tm) * reciprod))
!
!      par = apar * 4.6 ! (multiply w/m2 by 4.6 to get umol/m2/s)
!      cl  = alpha * cl1 * par * (1. + alpha * alpha * par * par)**(-0.5)
!
!   gamma(1) = cl * ct ! for isoprenes
!
!   do ivoc = 2, 5
!   gamma(ivoc) = exp(bet * (tv - tstd))
!   end do
!
! Foliage density
!
! transform vegfrac to lai      
!
!   elai    = max(0.0,-6.5/2.5*alog((1.-vegfrac)))
!   density = elai / (parameters%slarea(VEGTYP) * 0.5)
!
! calculate the voc flux
!
!   do ivoc = 1, 5
!   vocflx(ivoc) = epsilon(ivoc) * gamma(ivoc) * density
!   end do
!
!   end SUBROUTINE bvocflux
! ==================================================================================================

!***************** SUBROUTINES FOR GECROS CROP SIMULATION ***************
!*----------------------------------------------------------------------*
!*  SUBROUTINE EMERG                                                    *
!*  Purpose: This SUBROUTINE calculates germINation and emergence of    *
!*  the crop                                                            *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=INput,O=OUTput,C=control,IN=INit,T=time)     *
!*                                                                      *
!*  name   type meanINg                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  nowdate C12 Actual date                                dd.mm.yy  I  *
!*  DT      R4  Time step of INtegration                     s       I  *
!*  DD      R4  DrillINg depth                               cm      I  *
!*  TSOIL   R4  Soil temperature IN first layer              K       I  *
!*  TBEM    R4  Temperature threshold                        oC      I  *
!*  EMA     R4  Intercept of function for emergence          oC      I  *
!*  EMB     R4  Slope of function for emergence              oC      I  *
!*  TTEM    R4  Cumulative temperature sum for emergence     oC     I/O *
!*  EMERGENCE LOG Flag for emergence                          -     I/O *  
!*  STATE_GECROS(41) = emerged yes/no                                   *
!*  STATE_GECROS(43) = TTEM                                             *
!*----------------------------------------------------------------------*

SUBROUTINE EMERG(DT, TSOIL, DD, TBEM, EMA, EMB, STATE_GECROS)
      IMPLICIT NONE
      REAL, INTENT(IN) :: DT, TSOIL, DD, TBEM, EMA, EMB
      REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS
      REAL :: EMTH, TINT
      SAVE
      
      IF ((TSOIL-273.15).LT.TBEM) THEN
      ELSE
           STATE_GECROS(43) = STATE_GECROS(43) + (TSOIL-273.15-TBEM)/(86400./DT)
      ENDIF
      
      EMTH = EMA + EMB*DD
           
      IF (STATE_GECROS(43).GT.EMTH) THEN
         STATE_GECROS(41)=1.
!         write(*,*) 'Crop emerged on ', nowdate
!		 read(*,*)
      ELSE
         STATE_GECROS(41)=-1.
      ENDIF
      
      RETURN
END SUBROUTINE EMERG

! ********************************* end of carbon SUBROUTINEs *****************************
! ==================================================================================================

!== begIN noahmp_options ===========================================================================

  SUBROUTINE noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , & 
                             iopt_INf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, &
			     iopt_rsf , iopt_soil, iopt_pedo, iopt_crop )

  implicit none

  INTEGER,  INTENT(IN) :: idveg     !dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1
  INTEGER,  INTENT(IN) :: iopt_crs  !canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER,  INTENT(IN) :: iopt_btr  !soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER,  INTENT(IN) :: iopt_run  !runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER,  INTENT(IN) :: iopt_sfc  !surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER,  INTENT(IN) :: iopt_frz  !supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_INf  !frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_rad  !radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS 3->SNICAR)
  INTEGER,  INTENT(IN) :: iopt_snf  !raINfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)

  INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme (only layer 1)
                                    ! 1 -> semi-implicit; 2 -> full implicit (origINal Noah)
  INTEGER,  INTENT(IN) :: iopt_rsf  !surface resistance (1->Sakaguchi/Zeng; 2->Seller; 3->mod Sellers; 4->1+snow)
  INTEGER,  INTENT(IN) :: iopt_soil !soil parameters set-up option
  INTEGER,  INTENT(IN) :: iopt_pedo !pedo-transfer function
  INTEGER,  INTENT(IN) :: iopt_crop !crop model option (0->none; 1->Liu et al.; 2->Gecros)

! -------------------------------------------------------------------------------------------------

  dveg = idveg
  
  opt_crs  = iopt_crs  
  opt_btr  = iopt_btr  
  opt_run  = iopt_run  
  opt_sfc  = iopt_sfc  
  opt_frz  = iopt_frz  
  opt_INf  = iopt_INf  
  opt_rad  = iopt_rad  
  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  opt_rsf  = iopt_rsf
  opt_soil = iopt_soil
  opt_pedo = iopt_pedo
  opt_crop = iopt_crop
  
  end SUBROUTINE noahmp_options
 
END MODULE MODULE_SF_NOAHMPLSM

MODULE NOAHMP_TABLES

    IMPLICIT NONE

    INTEGER, PRIVATE, PARAMETER :: MVT   = 27
    INTEGER, PRIVATE, PARAMETER :: MBAND = 2
    INTEGER, PRIVATE, PARAMETER :: MSC   = 8
    INTEGER, PRIVATE, PARAMETER :: MAX_SOILTYP = 30
    INTEGER, PRIVATE, PARAMETER :: NCROP = 5
    INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8

! MPTABLE.TBL vegetation parameters

    INTEGER :: ISURBAN_TABLE
    INTEGER :: ISWATER_TABLE
    INTEGER :: ISBARREN_TABLE
    INTEGER :: ISICE_TABLE
    INTEGER :: ISCROP_TABLE
    INTEGER :: EBLFOREST_TABLE
    INTEGER :: NATURAL_TABLE
    INTEGER :: LOW_DENSITY_RESIDENTIAL_TABLE
    INTEGER :: HIGH_DENSITY_RESIDENTIAL_TABLE
    INTEGER :: HIGH_INTENSITY_INDUSTRIAL_TABLE

    REAL :: CH2OP_TABLE(MVT)       !maximum INtercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF_TABLE(MVT)       !characteristic leaf dimension (m)
    REAL :: Z0MVT_TABLE(MVT)       !momentum roughness length (m)
    REAL :: HVT_TABLE(MVT)         !top of canopy (m)
    REAL :: HVB_TABLE(MVT)         !bottom of canopy (m)
    REAL :: DEN_TABLE(MVT)         !tree density (no. of trunks per m2)
    REAL :: RC_TABLE(MVT)          !tree crown radius (m)
    REAL :: MFSNO_TABLE(MVT)       !snowmelt curve parameter ()
    REAL :: SAIM_TABLE(MVT,12)     !monthly stem area INdex, one-sided
    REAL :: LAIM_TABLE(MVT,12)     !monthly leaf area INdex, one-sided
    REAL :: SLA_TABLE(MVT)         !sINgle-side leaf area per Kg [m2/kg]
    REAL :: DILEFC_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: DILEFW_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: FRAGR_TABLE(MVT)       !fraction of growth respiration  !origINal was 0.3 
    REAL :: LTOVRC_TABLE(MVT)      !leaf turnover [1/s]

    REAL :: C3PSN_TABLE(MVT)       !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25_TABLE(MVT)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC_TABLE(MVT)         !q10 for kc25
    REAL :: KO25_TABLE(MVT)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO_TABLE(MVT)         !q10 for ko25
    REAL :: VCMX25_TABLE(MVT)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX_TABLE(MVT)       !q10 for vcmx25
    REAL :: BP_TABLE(MVT)          !mINimum leaf conductance (umol/m**2/s)
    REAL :: MP_TABLE(MVT)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25_TABLE(MVT)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE_TABLE(MVT)         !q10 for qe25
    REAL :: RMF25_TABLE(MVT)       !leaf maINtenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25_TABLE(MVT)       !stem maINtenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25_TABLE(MVT)       !root maINtenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM_TABLE(MVT)         !q10 for maINtenance respiration
    REAL :: FOLNMX_TABLE(MVT)      !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN_TABLE(MVT)        !mINimum temperature for photosynthesis (k)

    REAL :: XL_TABLE(MVT)          !leaf/stem orientation INdex
    REAL :: RHOL_TABLE(MVT,MBAND)  !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS_TABLE(MVT,MBAND)  !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL_TABLE(MVT,MBAND)  !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS_TABLE(MVT,MBAND)  !stem transmittance: 1=vis, 2=nir

    REAL :: MRP_TABLE(MVT)         !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT_TABLE(MVT)       !empirical canopy wINd parameter

    REAL :: WRRAT_TABLE(MVT)       !wood to non-wood ratio
    REAL :: WDPOOL_TABLE(MVT)      !wood pool (switch 1 or 0) dependINg on woody or not [-]
    REAL :: TDLEF_TABLE(MVT)       !characteristic T for leaf freezINg [K]

    REAL :: NROOT_TABLE(MVT)       !number of soil layers with root present
    REAL :: RGL_TABLE(MVT)         !Parameter used IN radiation stress function
    REAL :: RS_TABLE(MVT)          !MINimum stomatal resistance [s m-1]
    REAL :: HS_TABLE(MVT)          !Parameter used IN vapor pressure deficit function
    REAL :: TOPT_TABLE(MVT)        !Optimum transpiration air temperature [K]
    REAL :: RSMAX_TABLE(MVT)       !Maximal stomatal resistance [s m-1]

! SOILPARM.TBL parameters

    INTEGER            :: SLCATS

    REAL :: BEXP_TABLE(MAX_SOILTYP)          !maximum INtercepted h2o per unit lai+sai (mm)
    REAL :: SMCDRY_TABLE(MAX_SOILTYP)      !characteristic leaf dimension (m)
    REAL :: F1_TABLE(MAX_SOILTYP)         !momentum roughness length (m)
    REAL :: SMCMAX_TABLE(MAX_SOILTYP)      !top of canopy (m)
    REAL :: SMCREF_TABLE(MAX_SOILTYP)      !bottom of canopy (m)
    REAL :: PSISAT_TABLE(MAX_SOILTYP)      !tree density (no. of trunks per m2)
    REAL :: DKSAT_TABLE(MAX_SOILTYP)       !tree crown radius (m)
    REAL :: DWSAT_TABLE(MAX_SOILTYP)       !monthly stem area INdex, one-sided
    REAL :: SMCWLT_TABLE(MAX_SOILTYP)      !monthly leaf area INdex, one-sided
    REAL :: QUARTZ_TABLE(MAX_SOILTYP)         !sINgle-side leaf area per Kg [m2/kg]

! GENPARM.TBL parameters

    REAL :: SLOPE_TABLE(9)    !slope factor for soil draINage
    
    REAL :: CSOIL_TABLE       !Soil heat capacity [J m-3 K-1]
    REAL :: REFDK_TABLE       !Parameter IN the surface runoff parameterization
    REAL :: REFKDT_TABLE      !Parameter IN the surface runoff parameterization
    REAL :: FRZK_TABLE        !Frozen ground parameter
    REAL :: ZBOT_TABLE        !Depth [m] of lower boundary soil temperature
    REAL :: CZIL_TABLE        !Parameter used IN the calculation of the roughness length for heat

! MPTABLE.TBL radiation parameters

    REAL :: ALBSAT_TABLE(MSC,MBAND)   !saturated soil albedos: 1=vis, 2=nir
    REAL :: ALBDRY_TABLE(MSC,MBAND)   !dry soil albedos: 1=vis, 2=nir
    REAL :: ALBICE_TABLE(MBAND)       !albedo land ice: 1=vis, 2=nir
    REAL :: ALBLAK_TABLE(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
    REAL :: OMEGAS_TABLE(MBAND)       !two-stream parameter omega for snow
    REAL :: BETADS_TABLE              !two-stream parameter betad for snow
    REAL :: BETAIS_TABLE              !two-stream parameter betad for snow
    REAL :: EG_TABLE(2)               !emissivity

! MPTABLE.TBL global parameters

    REAL :: CO2_TABLE      !co2 partial pressure
    REAL :: O2_TABLE       !o2 partial pressure
    REAL :: TIMEAN_TABLE   !gridcell mean topgraphic INdex (global mean)
    REAL :: FSATMX_TABLE   !maximum surface saturated fraction (global mean)
    REAL :: Z0SNO_TABLE    !snow surface roughness length (m) (0.002)
    REAL :: SSI_TABLE      !liquid water holdINg capacity for snowpack (m3/m3) (0.03)
    REAL :: SWEMX_TABLE    !new snow mass to fully cover old snow (mm)
    REAL :: TAU0_TABLE          !tau0 from Yang97 eqn. 10a
    REAL :: GRAIN_GROWTH_TABLE  !growth from vapor diffusion Yang97 eqn. 10b
    REAL :: EXTRA_GROWTH_TABLE  !extra growth near freezINg Yang97 eqn. 10c
    REAL :: DIRT_SOOT_TABLE     !dirt and soot term Yang97 eqn. 10d
    REAL :: BATS_COSZ_TABLE     !zenith angle snow albedo adjustment; b IN Yang97 eqn. 15
    REAL :: BATS_VIS_NEW_TABLE  !new snow visible albedo
    REAL :: BATS_NIR_NEW_TABLE  !new snow NIR albedo
    REAL :: BATS_VIS_AGE_TABLE  !age factor for diffuse visible snow albedo Yang97 eqn. 17
    REAL :: BATS_NIR_AGE_TABLE  !age factor for diffuse NIR snow albedo Yang97 eqn. 18
    REAL :: BATS_VIS_DIR_TABLE  !cosz factor for direct visible snow albedo Yang97 eqn. 15
    REAL :: BATS_NIR_DIR_TABLE  !cosz factor for direct NIR snow albedo Yang97 eqn. 16
    REAL :: RSURF_SNOW_TABLE    !surface resistance for snow(s/m)
    REAL :: RSURF_EXP_TABLE    !exponent IN the shape parameter for soil resistance option 1

! MPTABLE.TBL crop parameters

 INTEGER :: DEFAULT_CROP_TABLE          ! Default crop INdex
 INTEGER :: PLTDAY_TABLE(NCROP)         ! PlantINg date
 INTEGER :: HSDAY_TABLE(NCROP)          ! Harvest date
    REAL :: PLANTPOP_TABLE(NCROP)       ! Plant density [per ha] - used?
    REAL :: IRRI_TABLE(NCROP)           ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)

    REAL :: GDDTBASE_TABLE(NCROP)       ! Base temperature for GDD accumulation [C]
    REAL :: GDDTCUT_TABLE(NCROP)        ! Upper temperature for GDD accumulation [C]
    REAL :: GDDS1_TABLE(NCROP)          ! GDD from seedINg to emergence
    REAL :: GDDS2_TABLE(NCROP)          ! GDD from seedINg to INitial vegetative 
    REAL :: GDDS3_TABLE(NCROP)          ! GDD from seedINg to post vegetative 
    REAL :: GDDS4_TABLE(NCROP)          ! GDD from seedINg to INtial reproductive
    REAL :: GDDS5_TABLE(NCROP)          ! GDD from seedINg to pysical maturity 

 INTEGER :: C3C4_TABLE(NCROP)           ! photosynthetic pathway:  1. = c3 2. = c4
    REAL :: AREF_TABLE(NCROP)           ! reference maximum CO2 assimulation rate 
    REAL :: PSNRF_TABLE(NCROP)          ! CO2 assimulation reduction factor(0-1) (caused by non-modelINg part,e.g.pest,weeds)
    REAL :: I2PAR_TABLE(NCROP)          ! Fraction of INcomINg solar radiation to photosynthetically active radiation
    REAL :: TASSIM0_TABLE(NCROP)        ! MINimum temperature for CO2 assimulation [C]
    REAL :: TASSIM1_TABLE(NCROP)        ! CO2 assimulation lINearly INcreasINg until temperature reaches T1 [C]
    REAL :: TASSIM2_TABLE(NCROP)        ! CO2 assmilation rate remaIN at Aref until temperature reaches T2 [C]
    REAL :: K_TABLE(NCROP)              ! light extINction coefficient
    REAL :: EPSI_TABLE(NCROP)           ! INitial light use efficiency

    REAL :: Q10MR_TABLE(NCROP)          ! q10 for maINtaINance respiration
    REAL :: FOLN_MX_TABLE(NCROP)        ! foliage nitrogen concentration when f(n)=1 (%)
    REAL :: LEFREEZ_TABLE(NCROP)        ! characteristic T for leaf freezINg [K]

    REAL :: DILE_FC_TABLE(NCROP,NSTAGE) ! coeficient for temperature leaf stress death [1/s]
    REAL :: DILE_FW_TABLE(NCROP,NSTAGE) ! coeficient for water leaf stress death [1/s]
    REAL :: FRA_GR_TABLE(NCROP)         ! fraction of growth respiration

    REAL :: LF_OVRC_TABLE(NCROP,NSTAGE) ! fraction of leaf turnover  [1/s]
    REAL :: ST_OVRC_TABLE(NCROP,NSTAGE) ! fraction of stem turnover  [1/s]
    REAL :: RT_OVRC_TABLE(NCROP,NSTAGE) ! fraction of root tunrover  [1/s]
    REAL :: LFMR25_TABLE(NCROP)         !  leaf maINtenance respiration at 25C [umol CO2/m**2  /s]
    REAL :: STMR25_TABLE(NCROP)         !  stem maINtenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: RTMR25_TABLE(NCROP)         !  root maINtenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: GRAINMR25_TABLE(NCROP)      ! graIN maINtenance respiration at 25C [umol CO2/kg bio/s]

    REAL :: LFPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to leaf
    REAL :: STPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to stem
    REAL :: RTPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to root
    REAL :: GRAINPT_TABLE(NCROP,NSTAGE) ! fraction of carbohydrate flux to graIN
    REAL :: BIO2LAI_TABLE(NCROP)        ! leaf are per livINg leaf biomass [m^2/kg]

! MPTABLE.TBL optional parameters

    REAL :: sr2006_theta_1500t_a        ! sand coefficient
    REAL :: sr2006_theta_1500t_b        ! clay coefficient
    REAL :: sr2006_theta_1500t_c        ! orgm coefficient
    REAL :: sr2006_theta_1500t_d        ! sand*orgm coefficient
    REAL :: sr2006_theta_1500t_e        ! clay*orgm coefficient
    REAL :: sr2006_theta_1500t_f        ! sand*clay coefficient
    REAL :: sr2006_theta_1500t_g        ! constant adjustment

    REAL :: sr2006_theta_1500_a         ! theta_1500t coefficient
    REAL :: sr2006_theta_1500_b         ! constant adjustment

    REAL :: sr2006_theta_33t_a          ! sand coefficient
    REAL :: sr2006_theta_33t_b          ! clay coefficient
    REAL :: sr2006_theta_33t_c          ! orgm coefficient
    REAL :: sr2006_theta_33t_d          ! sand*orgm coefficient
    REAL :: sr2006_theta_33t_e          ! clay*orgm coefficient
    REAL :: sr2006_theta_33t_f          ! sand*clay coefficient
    REAL :: sr2006_theta_33t_g          ! constant adjustment

    REAL :: sr2006_theta_33_a           ! theta_33t*theta_33t coefficient
    REAL :: sr2006_theta_33_b           ! theta_33t coefficient
    REAL :: sr2006_theta_33_c           ! constant adjustment

    REAL :: sr2006_theta_s33t_a         ! sand coefficient
    REAL :: sr2006_theta_s33t_b         ! clay coefficient
    REAL :: sr2006_theta_s33t_c         ! orgm coefficient
    REAL :: sr2006_theta_s33t_d         ! sand*orgm coefficient
    REAL :: sr2006_theta_s33t_e         ! clay*orgm coefficient
    REAL :: sr2006_theta_s33t_f         ! sand*clay coefficient
    REAL :: sr2006_theta_s33t_g         ! constant adjustment

    REAL :: sr2006_theta_s33_a          ! theta_s33t coefficient
    REAL :: sr2006_theta_s33_b          ! constant adjustment

    REAL :: sr2006_psi_et_a             ! sand coefficient
    REAL :: sr2006_psi_et_b             ! clay coefficient
    REAL :: sr2006_psi_et_c             ! theta_s33 coefficient
    REAL :: sr2006_psi_et_d             ! sand*theta_s33 coefficient
    REAL :: sr2006_psi_et_e             ! clay*theta_s33 coefficient
    REAL :: sr2006_psi_et_f             ! sand*clay coefficient
    REAL :: sr2006_psi_et_g             ! constant adjustment

    REAL :: sr2006_psi_e_a              ! psi_et*psi_et coefficient
    REAL :: sr2006_psi_e_b              ! psi_et coefficient
    REAL :: sr2006_psi_e_c              ! constant adjustment

    REAL :: sr2006_smcmax_a             ! sand adjustment
    REAL :: sr2006_smcmax_b             ! constant adjustment

CONTAINS

  SUBROUTINE read_mp_veg_parameters(DATASET_IDENTIFIER)
    implicit none
    character(len=*), INTENT(IN) :: DATASET_IDENTIFIER
    INTEGER :: ierr
    INTEGER :: IK,IM
    logical :: file_named 

    INTEGER :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    INTEGER :: ISURBAN
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST
    INTEGER :: NATURAL
    INTEGER :: LOW_DENSITY_RESIDENTIAL
    INTEGER :: HIGH_DENSITY_RESIDENTIAL
    INTEGER :: HIGH_INTENSITY_INDUSTRIAL

    REAL, DIMENSION(MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    REAL, DIMENSION(MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    REAL, DIMENSION(MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                     TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    REAL, DIMENSION(MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                     AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                     BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
		     SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
			     
    NAMELIST / noahmp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
	 
    NAMELIST / noahmp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screechINg halt as soon as we try to use anythINg.
    CH2OP_TABLE  = -1.E36
    DLEAF_TABLE  = -1.E36
    Z0MVT_TABLE  = -1.E36
    HVT_TABLE    = -1.E36
    HVB_TABLE    = -1.E36
    DEN_TABLE    = -1.E36
    RC_TABLE     = -1.E36
    MFSNO_TABLE  = -1.E36
    RHOL_TABLE   = -1.E36
    RHOS_TABLE   = -1.E36
    TAUL_TABLE   = -1.E36
    TAUS_TABLE   = -1.E36
    XL_TABLE     = -1.E36
    CWPVT_TABLE  = -1.E36
    C3PSN_TABLE  = -1.E36
    KC25_TABLE   = -1.E36
    AKC_TABLE    = -1.E36
    KO25_TABLE   = -1.E36
    AKO_TABLE    = -1.E36
    AVCMX_TABLE  = -1.E36
    AQE_TABLE    = -1.E36
    LTOVRC_TABLE = -1.E36
    DILEFC_TABLE = -1.E36
    DILEFW_TABLE = -1.E36
    RMF25_TABLE  = -1.E36
    SLA_TABLE    = -1.E36
    FRAGR_TABLE  = -1.E36
    TMIN_TABLE   = -1.E36
    VCMX25_TABLE = -1.E36
    TDLEF_TABLE  = -1.E36
    BP_TABLE     = -1.E36
    MP_TABLE     = -1.E36
    QE25_TABLE   = -1.E36
    RMS25_TABLE  = -1.E36
    RMR25_TABLE  = -1.E36
    ARM_TABLE    = -1.E36
    FOLNMX_TABLE = -1.E36
    WDPOOL_TABLE = -1.E36
    WRRAT_TABLE  = -1.E36
    MRP_TABLE    = -1.E36
    SAIM_TABLE   = -1.E36
    LAIM_TABLE   = -1.E36
    NROOT_TABLE  = -1.E36
    RGL_TABLE    = -1.E36
    RS_TABLE     = -1.E36
    HS_TABLE     = -1.E36
    TOPT_TABLE   = -1.E36
    RSMAX_TABLE  = -1.E36
    ISURBAN_TABLE      = -99999
    ISWATER_TABLE      = -99999
    ISBARREN_TABLE     = -99999
    ISICE_TABLE        = -99999
    ISCROP_TABLE       = -99999
    EBLFOREST_TABLE    = -99999
    NATURAL_TABLE      = -99999
    LOW_DENSITY_RESIDENTIAL_TABLE   = -99999
    HIGH_DENSITY_RESIDENTIAL_TABLE  = -99999
    HIGH_INTENSITY_INDUSTRIAL_TABLE = -99999

    INquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot fINd file MPTABLE.TBL")')
       call wrf_error_fatal("STOP IN Noah-MP read_mp_veg_parameters")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noahmp_usgs_veg_categories)
       read(15,noahmp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("WARNING: Unrecognized DATASET_IDENTIFIER IN SUBROUTINE READ_MP_VEG_PARAMETERS")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call wrf_error_fatal("STOP IN Noah-MP read_mp_veg_parameters")
    endif
    close(15)

                      ISURBAN_TABLE   = ISURBAN
                      ISWATER_TABLE   = ISWATER
                     ISBARREN_TABLE   = ISBARREN
                        ISICE_TABLE   = ISICE
                       ISCROP_TABLE   = ISCROP
                    EBLFOREST_TABLE   = EBLFOREST
                      NATURAL_TABLE   = NATURAL
      LOW_DENSITY_RESIDENTIAL_TABLE   = LOW_DENSITY_RESIDENTIAL
     HIGH_DENSITY_RESIDENTIAL_TABLE   = HIGH_DENSITY_RESIDENTIAL
    HIGH_INTENSITY_INDUSTRIAL_TABLE   = HIGH_INTENSITY_INDUSTRIAL

     CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
     DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
     Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
       HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
       HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
       DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
        RC_TABLE(1:NVEG)  = RC(1:NVEG)
     MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
        XL_TABLE(1:NVEG)  = XL(1:NVEG)
     CWPVT_TABLE(1:NVEG)  = CWPVT(1:NVEG)
     C3PSN_TABLE(1:NVEG)  = C3PSN(1:NVEG)
      KC25_TABLE(1:NVEG)  = KC25(1:NVEG)
       AKC_TABLE(1:NVEG)  = AKC(1:NVEG)
      KO25_TABLE(1:NVEG)  = KO25(1:NVEG)
       AKO_TABLE(1:NVEG)  = AKO(1:NVEG)
     AVCMX_TABLE(1:NVEG)  = AVCMX(1:NVEG)
       AQE_TABLE(1:NVEG)  = AQE(1:NVEG)
    LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
     RMF25_TABLE(1:NVEG)  = RMF25(1:NVEG)
       SLA_TABLE(1:NVEG)  = SLA(1:NVEG)
     FRAGR_TABLE(1:NVEG)  = FRAGR(1:NVEG)
      TMIN_TABLE(1:NVEG)  = TMIN(1:NVEG)
    VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
     TDLEF_TABLE(1:NVEG)  = TDLEF(1:NVEG)
        BP_TABLE(1:NVEG)  = BP(1:NVEG)
        MP_TABLE(1:NVEG)  = MP(1:NVEG)
      QE25_TABLE(1:NVEG)  = QE25(1:NVEG)
     RMS25_TABLE(1:NVEG)  = RMS25(1:NVEG)
     RMR25_TABLE(1:NVEG)  = RMR25(1:NVEG)
       ARM_TABLE(1:NVEG)  = ARM(1:NVEG)
    FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
     WRRAT_TABLE(1:NVEG)  = WRRAT(1:NVEG)
       MRP_TABLE(1:NVEG)  = MRP(1:NVEG)
     NROOT_TABLE(1:NVEG)  = NROOT(1:NVEG)
       RGL_TABLE(1:NVEG)  = RGL(1:NVEG)
        RS_TABLE(1:NVEG)  = RS(1:NVEG)
        HS_TABLE(1:NVEG)  = HS(1:NVEG)
      TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
     RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)
    
    ! Put LAI and SAI INto 2d array from monthly lINes IN table; same for canopy radiation properties

    SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)

    LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)

    RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

  end SUBROUTINE read_mp_veg_parameters

  SUBROUTINE read_mp_soil_parameters()
    IMPLICIT NONE
    INTEGER :: IERR
    CHARACTER*4         :: SLTYPE
    INTEGER             :: ITMP, NUM_SLOPE, LC
    CHARACTER(len=256)  :: message
    logical             :: file_named 
    

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screechINg halt as soon as we try to use anythINg.
       BEXP_TABLE = -1.E36
     SMCDRY_TABLE = -1.E36
         F1_TABLE = -1.E36
     SMCMAX_TABLE = -1.E36
     SMCREF_TABLE = -1.E36
     PSISAT_TABLE = -1.E36
      DKSAT_TABLE = -1.E36
      DWSAT_TABLE = -1.E36
     SMCWLT_TABLE = -1.E36
     QUARTZ_TABLE = -1.E36
      SLOPE_TABLE = -1.E36
      CSOIL_TABLE = -1.E36
      REFDK_TABLE = -1.E36
     REFKDT_TABLE = -1.E36
       FRZK_TABLE = -1.E36
       ZBOT_TABLE = -1.E36
       CZIL_TABLE = -1.E36

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
    INquire( file='SOILPARM.TBL', exist=file_named ) 
    if ( file_named ) then
      open(21, file='SOILPARM.TBL',form='formatted',status='old',iostat=ierr)
    else
      open(21, form='formatted',status='old',iostat=ierr)
    end if

    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahmpdrv.F: read_mp_soil_parameters: failure openINg SOILPARM.TBL'
      CALL wrf_error_fatal ( message )
    END IF

    READ (21,*)
    READ (21,*) SLTYPE
    READ (21,*) SLCATS
    WRITE( message , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
               SLCATS,' CATEGORIES'
    CALL wrf_message ( message )

    DO LC=1,SLCATS
      READ (21,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),    &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC), QUARTZ_TABLE(LC)
    ENDDO

    CLOSE (21)

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    INquire( file='GENPARM.TBL', exist=file_named ) 
    if ( file_named ) then
      open(22, file='GENPARM.TBL',form='formatted',status='old',iostat=ierr)
    else
      open(22, form='formatted',status='old',iostat=ierr)
    end if

    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahlsm.F: read_mp_soil_parameters: failure openINg GENPARM.TBL'
      CALL wrf_error_fatal ( message )
    END IF

    READ (22,*)
    READ (22,*)
    READ (22,*) NUM_SLOPE

    DO LC=1,NUM_SLOPE
        READ (22,*) SLOPE_TABLE(LC)
    ENDDO

    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*) CSOIL_TABLE
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*) REFDK_TABLE
    READ (22,*)
    READ (22,*) REFKDT_TABLE
    READ (22,*)
    READ (22,*) FRZK_TABLE
    READ (22,*)
    READ (22,*) ZBOT_TABLE
    READ (22,*)
    READ (22,*) CZIL_TABLE
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)

    CLOSE (22)

  end SUBROUTINE read_mp_soil_parameters

  SUBROUTINE read_mp_rad_parameters()
    implicit none
    INTEGER :: ierr
    logical :: file_named 

    REAL :: ALBICE(MBAND),ALBLAK(MBAND),OMEGAS(MBAND),BETADS,BETAIS,EG(2)
    REAL :: ALBSAT_VIS(MSC)
    REAL :: ALBSAT_NIR(MSC)
    REAL :: ALBDRY_VIS(MSC)
    REAL :: ALBDRY_NIR(MSC)

    NAMELIST / noahmp_rad_parameters / ALBSAT_VIS,ALBSAT_NIR,ALBDRY_VIS,ALBDRY_NIR,ALBICE,ALBLAK,OMEGAS,BETADS,BETAIS,EG


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screechINg halt as soon as we try to use anythINg.
    ALBSAT_TABLE     = -1.E36
    ALBDRY_TABLE     = -1.E36
    ALBICE_TABLE     = -1.E36
    ALBLAK_TABLE     = -1.E36
    OMEGAS_TABLE     = -1.E36
    BETADS_TABLE     = -1.E36
    BETAIS_TABLE     = -1.E36
    EG_TABLE         = -1.E36

    INquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot fINd file MPTABLE.TBL")')
       call wrf_error_fatal("STOP IN Noah-MP read_mp_rad_parameters")
    endif

    read(15,noahmp_rad_parameters)
    close(15)

    ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    ALBICE_TABLE      = ALBICE
    ALBLAK_TABLE      = ALBLAK
    OMEGAS_TABLE      = OMEGAS
    BETADS_TABLE      = BETADS
    BETAIS_TABLE      = BETAIS
    EG_TABLE          = EG

  end SUBROUTINE read_mp_rad_parameters

  SUBROUTINE read_mp_global_parameters()
    implicit none
    INTEGER :: ierr
    logical :: file_named 

    REAL :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI, &
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
	    BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
	    RSURF_SNOW,RSURF_EXP

    NAMELIST / noahmp_global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI, &
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
	    BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
	    RSURF_SNOW,RSURF_EXP


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screechINg halt as soon as we try to use anythINg.
       CO2_TABLE     = -1.E36
        O2_TABLE     = -1.E36
    TIMEAN_TABLE     = -1.E36
    FSATMX_TABLE     = -1.E36
     Z0SNO_TABLE     = -1.E36
       SSI_TABLE     = -1.E36
     SWEMX_TABLE     = -1.E36
        TAU0_TABLE   = -1.E36
GRAIN_GROWTH_TABLE   = -1.E36
EXTRA_GROWTH_TABLE   = -1.E36
   DIRT_SOOT_TABLE   = -1.E36
   BATS_COSZ_TABLE   = -1.E36
BATS_VIS_NEW_TABLE   = -1.E36
BATS_NIR_NEW_TABLE   = -1.E36
BATS_VIS_AGE_TABLE   = -1.E36
BATS_NIR_AGE_TABLE   = -1.E36
BATS_VIS_DIR_TABLE   = -1.E36
BATS_NIR_DIR_TABLE   = -1.E36
RSURF_SNOW_TABLE     = -1.E36
 RSURF_EXP_TABLE     = -1.E36

    INquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot fINd file MPTABLE.TBL")')
       call wrf_error_fatal("STOP IN Noah-MP read_mp_global_parameters")
    endif

    read(15,noahmp_global_parameters)
    close(15)

       CO2_TABLE     = CO2
        O2_TABLE     = O2
    TIMEAN_TABLE     = TIMEAN
    FSATMX_TABLE     = FSATMX
     Z0SNO_TABLE     = Z0SNO
       SSI_TABLE     = SSI
     SWEMX_TABLE     = SWEMX
        TAU0_TABLE   = TAU0
GRAIN_GROWTH_TABLE   = GRAIN_GROWTH
EXTRA_GROWTH_TABLE   = EXTRA_GROWTH
   DIRT_SOOT_TABLE   = DIRT_SOOT
   BATS_COSZ_TABLE   = BATS_COSZ
BATS_VIS_NEW_TABLE   = BATS_VIS_NEW
BATS_NIR_NEW_TABLE   = BATS_NIR_NEW
BATS_VIS_AGE_TABLE   = BATS_VIS_AGE
BATS_NIR_AGE_TABLE   = BATS_NIR_AGE
BATS_VIS_DIR_TABLE   = BATS_VIS_DIR
BATS_NIR_DIR_TABLE   = BATS_NIR_DIR
RSURF_SNOW_TABLE     = RSURF_SNOW
 RSURF_EXP_TABLE     = RSURF_EXP

  end SUBROUTINE read_mp_global_parameters

  SUBROUTINE read_mp_crop_parameters()
    implicit none
    INTEGER :: ierr
    logical :: file_named 

 INTEGER                   :: DEFAULT_CROP
 INTEGER, DIMENSION(NCROP) :: PLTDAY
 INTEGER, DIMENSION(NCROP) :: HSDAY
    REAL, DIMENSION(NCROP) :: PLANTPOP
    REAL, DIMENSION(NCROP) :: IRRI
    REAL, DIMENSION(NCROP) :: GDDTBASE
    REAL, DIMENSION(NCROP) :: GDDTCUT
    REAL, DIMENSION(NCROP) :: GDDS1
    REAL, DIMENSION(NCROP) :: GDDS2
    REAL, DIMENSION(NCROP) :: GDDS3
    REAL, DIMENSION(NCROP) :: GDDS4
    REAL, DIMENSION(NCROP) :: GDDS5
 INTEGER, DIMENSION(NCROP) :: C3C4
    REAL, DIMENSION(NCROP) :: AREF
    REAL, DIMENSION(NCROP) :: PSNRF
    REAL, DIMENSION(NCROP) :: I2PAR
    REAL, DIMENSION(NCROP) :: TASSIM0
    REAL, DIMENSION(NCROP) :: TASSIM1
    REAL, DIMENSION(NCROP) :: TASSIM2
    REAL, DIMENSION(NCROP) :: K
    REAL, DIMENSION(NCROP) :: EPSI
    REAL, DIMENSION(NCROP) :: Q10MR
    REAL, DIMENSION(NCROP) :: FOLN_MX
    REAL, DIMENSION(NCROP) :: LEFREEZ
    REAL, DIMENSION(NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    REAL, DIMENSION(NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    REAL, DIMENSION(NCROP) :: FRA_GR
    REAL, DIMENSION(NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    REAL, DIMENSION(NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    REAL, DIMENSION(NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    REAL, DIMENSION(NCROP) :: LFMR25
    REAL, DIMENSION(NCROP) :: STMR25
    REAL, DIMENSION(NCROP) :: RTMR25
    REAL, DIMENSION(NCROP) :: GRAINMR25
    REAL, DIMENSION(NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    REAL, DIMENSION(NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    REAL, DIMENSION(NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    REAL, DIMENSION(NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    REAL, DIMENSION(NCROP) :: BIO2LAI


    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,     GDDS2, &
                                             GDDS3,     GDDS4,     GDDS5,      C3C4,      AREF,     PSNRF,     I2PAR,   TASSIM0, &
                                           TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,            &
                                        DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8, &
                                        DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8, &
                                            FRA_GR,                                                                              &
                                        LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8, &
                                        ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8, &
                                        RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8, &
                                            LFMR25,    STMR25,    RTMR25, GRAINMR25,                                             &
                                           LFPT_S1,   LFPT_S2,   LFPT_S3,   LFPT_S4,   LFPT_S5,   LFPT_S6,   LFPT_S7,   LFPT_S8, &
                                           STPT_S1,   STPT_S2,   STPT_S3,   STPT_S4,   STPT_S5,   STPT_S6,   STPT_S7,   STPT_S8, &
                                           RTPT_S1,   RTPT_S2,   RTPT_S3,   RTPT_S4,   RTPT_S5,   RTPT_S6,   RTPT_S7,   RTPT_S8, &
                                        GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8, &
                                           BIO2LAI


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screechINg halt as soon as we try to use anythINg.
 DEFAULT_CROP_TABLE     = -99999
       PLTDAY_TABLE     = -99999
        HSDAY_TABLE     = -99999
     PLANTPOP_TABLE     = -1.E36
         IRRI_TABLE     = -1.E36
     GDDTBASE_TABLE     = -1.E36
      GDDTCUT_TABLE     = -1.E36
        GDDS1_TABLE     = -1.E36
        GDDS2_TABLE     = -1.E36
        GDDS3_TABLE     = -1.E36
        GDDS4_TABLE     = -1.E36
        GDDS5_TABLE     = -1.E36
         C3C4_TABLE     = -99999
         AREF_TABLE     = -1.E36
        PSNRF_TABLE     = -1.E36
        I2PAR_TABLE     = -1.E36
      TASSIM0_TABLE     = -1.E36
      TASSIM1_TABLE     = -1.E36
      TASSIM2_TABLE     = -1.E36
            K_TABLE     = -1.E36
         EPSI_TABLE     = -1.E36
        Q10MR_TABLE     = -1.E36
      FOLN_MX_TABLE     = -1.E36
      LEFREEZ_TABLE     = -1.E36
      DILE_FC_TABLE     = -1.E36
      DILE_FW_TABLE     = -1.E36
       FRA_GR_TABLE     = -1.E36
      LF_OVRC_TABLE     = -1.E36
      ST_OVRC_TABLE     = -1.E36
      RT_OVRC_TABLE     = -1.E36
       LFMR25_TABLE     = -1.E36
       STMR25_TABLE     = -1.E36
       RTMR25_TABLE     = -1.E36
    GRAINMR25_TABLE     = -1.E36
         LFPT_TABLE     = -1.E36
         STPT_TABLE     = -1.E36
         RTPT_TABLE     = -1.E36
      GRAINPT_TABLE     = -1.E36
      BIO2LAI_TABLE     = -1.E36


    INquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot fINd file MPTABLE.TBL")')
       call wrf_error_fatal("STOP IN Noah-MP read_mp_crop_parameters")
    endif

    read(15,noahmp_crop_parameters)
    close(15)

 DEFAULT_CROP_TABLE      = DEFAULT_CROP
       PLTDAY_TABLE      = PLTDAY
        HSDAY_TABLE      = HSDAY
     PLANTPOP_TABLE      = PLANTPOP
         IRRI_TABLE      = IRRI
     GDDTBASE_TABLE      = GDDTBASE
      GDDTCUT_TABLE      = GDDTCUT
        GDDS1_TABLE      = GDDS1
        GDDS2_TABLE      = GDDS2
        GDDS3_TABLE      = GDDS3
        GDDS4_TABLE      = GDDS4
        GDDS5_TABLE      = GDDS5
         C3C4_TABLE      = C3C4
         AREF_TABLE      = AREF
        PSNRF_TABLE      = PSNRF
        I2PAR_TABLE      = I2PAR
      TASSIM0_TABLE      = TASSIM0
      TASSIM1_TABLE      = TASSIM1
      TASSIM2_TABLE      = TASSIM2
            K_TABLE      = K
         EPSI_TABLE      = EPSI
        Q10MR_TABLE      = Q10MR
      FOLN_MX_TABLE      = FOLN_MX
      LEFREEZ_TABLE      = LEFREEZ
      DILE_FC_TABLE(:,1) = DILE_FC_S1
      DILE_FC_TABLE(:,2) = DILE_FC_S2
      DILE_FC_TABLE(:,3) = DILE_FC_S3
      DILE_FC_TABLE(:,4) = DILE_FC_S4
      DILE_FC_TABLE(:,5) = DILE_FC_S5
      DILE_FC_TABLE(:,6) = DILE_FC_S6
      DILE_FC_TABLE(:,7) = DILE_FC_S7
      DILE_FC_TABLE(:,8) = DILE_FC_S8
      DILE_FW_TABLE(:,1) = DILE_FW_S1
      DILE_FW_TABLE(:,2) = DILE_FW_S2
      DILE_FW_TABLE(:,3) = DILE_FW_S3
      DILE_FW_TABLE(:,4) = DILE_FW_S4
      DILE_FW_TABLE(:,5) = DILE_FW_S5
      DILE_FW_TABLE(:,6) = DILE_FW_S6
      DILE_FW_TABLE(:,7) = DILE_FW_S7
      DILE_FW_TABLE(:,8) = DILE_FW_S8
       FRA_GR_TABLE      = FRA_GR
      LF_OVRC_TABLE(:,1) = LF_OVRC_S1
      LF_OVRC_TABLE(:,2) = LF_OVRC_S2
      LF_OVRC_TABLE(:,3) = LF_OVRC_S3
      LF_OVRC_TABLE(:,4) = LF_OVRC_S4
      LF_OVRC_TABLE(:,5) = LF_OVRC_S5
      LF_OVRC_TABLE(:,6) = LF_OVRC_S6
      LF_OVRC_TABLE(:,7) = LF_OVRC_S7
      LF_OVRC_TABLE(:,8) = LF_OVRC_S8
      ST_OVRC_TABLE(:,1) = ST_OVRC_S1
      ST_OVRC_TABLE(:,2) = ST_OVRC_S2
      ST_OVRC_TABLE(:,3) = ST_OVRC_S3
      ST_OVRC_TABLE(:,4) = ST_OVRC_S4
      ST_OVRC_TABLE(:,5) = ST_OVRC_S5
      ST_OVRC_TABLE(:,6) = ST_OVRC_S6
      ST_OVRC_TABLE(:,7) = ST_OVRC_S7
      ST_OVRC_TABLE(:,8) = ST_OVRC_S8
      RT_OVRC_TABLE(:,1) = RT_OVRC_S1
      RT_OVRC_TABLE(:,2) = RT_OVRC_S2
      RT_OVRC_TABLE(:,3) = RT_OVRC_S3
      RT_OVRC_TABLE(:,4) = RT_OVRC_S4
      RT_OVRC_TABLE(:,5) = RT_OVRC_S5
      RT_OVRC_TABLE(:,6) = RT_OVRC_S6
      RT_OVRC_TABLE(:,7) = RT_OVRC_S7
      RT_OVRC_TABLE(:,8) = RT_OVRC_S8
       LFMR25_TABLE      = LFMR25
       STMR25_TABLE      = STMR25
       RTMR25_TABLE      = RTMR25
    GRAINMR25_TABLE      = GRAINMR25
         LFPT_TABLE(:,1) = LFPT_S1
         LFPT_TABLE(:,2) = LFPT_S2
         LFPT_TABLE(:,3) = LFPT_S3
         LFPT_TABLE(:,4) = LFPT_S4
         LFPT_TABLE(:,5) = LFPT_S5
         LFPT_TABLE(:,6) = LFPT_S6
         LFPT_TABLE(:,7) = LFPT_S7
         LFPT_TABLE(:,8) = LFPT_S8
         STPT_TABLE(:,1) = STPT_S1
         STPT_TABLE(:,2) = STPT_S2
         STPT_TABLE(:,3) = STPT_S3
         STPT_TABLE(:,4) = STPT_S4
         STPT_TABLE(:,5) = STPT_S5
         STPT_TABLE(:,6) = STPT_S6
         STPT_TABLE(:,7) = STPT_S7
         STPT_TABLE(:,8) = STPT_S8
         RTPT_TABLE(:,1) = RTPT_S1
         RTPT_TABLE(:,2) = RTPT_S2
         RTPT_TABLE(:,3) = RTPT_S3
         RTPT_TABLE(:,4) = RTPT_S4
         RTPT_TABLE(:,5) = RTPT_S5
         RTPT_TABLE(:,6) = RTPT_S6
         RTPT_TABLE(:,7) = RTPT_S7
         RTPT_TABLE(:,8) = RTPT_S8
      GRAINPT_TABLE(:,1) = GRAINPT_S1
      GRAINPT_TABLE(:,2) = GRAINPT_S2
      GRAINPT_TABLE(:,3) = GRAINPT_S3
      GRAINPT_TABLE(:,4) = GRAINPT_S4
      GRAINPT_TABLE(:,5) = GRAINPT_S5
      GRAINPT_TABLE(:,6) = GRAINPT_S6
      GRAINPT_TABLE(:,7) = GRAINPT_S7
      GRAINPT_TABLE(:,8) = GRAINPT_S8
      BIO2LAI_TABLE      = BIO2LAI

  end SUBROUTINE read_mp_crop_parameters

  SUBROUTINE read_mp_optional_parameters()
    implicit none
    INTEGER :: ierr
    logical :: file_named 

    NAMELIST / noahmp_optional_parameters /                                &
         sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c, &
         sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f, &
         sr2006_theta_1500t_g                                            , &
         sr2006_theta_1500_a , sr2006_theta_1500_b                       , &
         sr2006_theta_33t_a  , sr2006_theta_33t_b  , sr2006_theta_33t_c  , &
         sr2006_theta_33t_d  , sr2006_theta_33t_e  , sr2006_theta_33t_f  , &
         sr2006_theta_33t_g                                              , &
         sr2006_theta_33_a   , sr2006_theta_33_b   , sr2006_theta_33_c   , &
         sr2006_theta_s33t_a , sr2006_theta_s33t_b , sr2006_theta_s33t_c , &
         sr2006_theta_s33t_d , sr2006_theta_s33t_e , sr2006_theta_s33t_f , &
         sr2006_theta_s33t_g                                             , &
         sr2006_theta_s33_a  , sr2006_theta_s33_b                        , &
         sr2006_psi_et_a     , sr2006_psi_et_b     , sr2006_psi_et_c     , &
         sr2006_psi_et_d     , sr2006_psi_et_e     , sr2006_psi_et_f     , &
         sr2006_psi_et_g                                                 , &
         sr2006_psi_e_a      , sr2006_psi_e_b      , sr2006_psi_e_c      , &
         sr2006_smcmax_a     , sr2006_smcmax_b

    INquire( file='MPTABLE.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot fINd file MPTABLE.TBL")')
       call wrf_error_fatal("STOP IN Noah-MP read_mp_optional_parameters")
    endif

    read(15,noahmp_optional_parameters)
    close(15)


  end SUBROUTINE read_mp_optional_parameters

END MODULE NOAHMP_TABLES

