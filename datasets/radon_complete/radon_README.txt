From Phil Price, 29 Nov 2017:

The LBNL high-radon data page was just taken down a couple of years ago — which probably makes sense, since the data it contained were 25-30 years old and a lot of new houses have been built in new places since then, which could alter the statistical distribution of radon by area.

I did manage to dig up the data on an old backup drive, though, so I have attached what I have:

1. Five data files from the EPA/State Residential Radon Surveys. These are comma-delimited files, srrsN.dat where N is 1 through 5.
2. A pdf that has the codebook that says what each data field contains. SRRSdoc.pdf
3. A subset of the data from the National Residential Radon Survey; this has all of the measurements, but not all of the data fields. nrrs.all.txt
4. A file that maps the “primary sampling unit” = “psu” from the NRRS data onto the county containing that psu

A quick search didn’t turn up a codebook for the NRRS data, although I probably have one. Below is what I remember, or believe I remember. If it is very important I can try to find more info, let me know.

stabbr, state abbreviation
psuid, primary sampling unit id
wt_nrdad, some sort of sampling weight (supposed to be the number of residences in the sampling frame that this house represents, I think)
lolv, radon measurement (in pCi/L) on the lowest living level of the house
lnlv, radon measurement on the lowest level of the home (whether used as living space or not) don’t ask me why they used this abbreviation
hhmn, household mean radon concentration (over all occupied levels of the home)
lolvnliv, is the lowest level non-living space (1=Yes, 2=No)
r_type, residence type… sorry I don’t remember exactly what this means
grnd_cn, is this a ground-contact home (i.e. is lowest level in contact with the ground) 
singmult, single- or multi-family dwelling? Hard to believe but I think they used 1=multi-family, 2=single.
r_res3, don’t remember
lowflor, don’t remember
bldg_age, some sort of building age category (probably 0-10 years, 11-20, etc., with some category for more than N years, but I don’t remember exactly)
r_basemt, whether the home has a basement,  1=yes I think.
basentr, is there a doorway between the basement and the first floor of the house
gaswthtr, is there a gas water heater
tight, must be related to airtightness or weatherstripping but I don’t remember
ventfreq, related to how often doors and windows are opened but I don’t remember.
