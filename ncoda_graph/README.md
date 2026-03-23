# Steps:
1. build using: `ncoda_graph/build.sh`
2. Make sure the `bin` directory is populated as shown below.
```
mkdir -p ../../../bin
cp /scratch4/NCEPDEV/marine/Santha.Akella/bin/* ../../../bin/
```
3. To generate plots: `cd ush-plots`

`./month_stats.sh 2026031000 10 v2.5 |& tee test.log`

or `./month_stats.sh 2026031500 15 v2.5`.

- Note the dates (input arguments).


4. Realize that `month_stats.sh` needs a database that contains staged output from NCODA:
- OCN_OUTPUT_DIR.
  - Use `fetch_rtofs_hpss_full.sh` for any date; which has a wrapper: `auto_fetch_rtofs.sh` for a date range.
  - Once data has been fetched, populate the directory: `make_database.sh`.
- OCN_CLIM_DIR 
  - Contents are static; have been copied over from: `/scratch4/NCEPDEV/marine/Zulema.Garraffo/ncoda/fix/codaclim`

# Details of above step 2 follow:
- gs: cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/bin/gs
- pstopng2.sh: cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/bin/pstopng2.sh
- gmeta2png.sh cp /scratch4/NCEPDEV/marine/Zulema.Garraffo/rtofs.prod.v2.5/ush_plot/gmeta2png.sh
- rtofs_dtg: builds from https://github.com/NOAA-EMC/NCODA/tree/develop/dtg/

5. Note that `month_stats.sh

# The expected location of the above:
```
[Santha.Akella@ufe01 ncoda_graph]$ pwd -P
/scratch4/NCEPDEV/marine/Santha.Akella/rtofs.prod.v2.5/rtofs-tools/ncoda_graph
[Santha.Akella@ufe01 ncoda_graph]$ 
[Santha.Akella@ufe01 ncoda_graph]$ ls ../../../bin/
README.md  gmeta2png.sh  gs  pstopng2.sh  rtofs_dtg  setup_env.sh
```
