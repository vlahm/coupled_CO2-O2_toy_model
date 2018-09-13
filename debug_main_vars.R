    tm <<- temperature; l <<- localHour; d <<- DOY;
    tt <<- TAlk; pp <<- pCO2_init; o <<- O2_init;  d <<- depth;
    aa <<- alphaPI_hr; r <<- Rstd_day; k <<- kstd_hr; lt <<- lat; ln <<- long;
    PAR <<- 'n'; pCO2_air <<- 410; referenceT <<- 20; salinity <<- 0; masl <<- 0

    temperature = tm; localHour = l; DOY = d;
    TAlk = tt; pCO2_init = pp; O2_init = o;
    alphaPI_hr = aa; Rstd_day = r; kstd_hr = k
    lat = lt; long = ln; depth = d

    # x.lt = as.POSIXlt(FishtrapCr$dateTime)
    # temperature = FishtrapCr$Temp_C
    # PAR = FishtrapCr$PAR_uE
    # localHour = x.lt$hour + x.lt$min/60 + x.lt$sec/3600
    # DOY = x.lt$yday
    # lat = 48.93861111
    # long = -122.47861111
    tz = -8
    masl = 1
    salinity = 0
