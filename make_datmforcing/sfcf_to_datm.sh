#!/bin/bash
#=====================================================================
# sfcf_to_datm.sh: make daily DATM forcing files from FV3 sfcf history
# files (one forecast hour per file), with NCO, and the datm.streams
# file (ESMF config format) that describes them.
#
# Two kinds of field, in two streams:
#   instantaneous   values at the output time (temperature, winds, ...):
#                   stream 01, files <prefix>_YYYYMMDD.nc, time = output hour
#   interval means  any field that is, or is computed from, a *_ave field
#                   (shortwave bands, precipitation): stream 02, files
#                   <prefix>_mean_YYYYMMDD.nc, time = the middle of the
#                   interval the mean covers (f000.5, f001.5, ... and
#                   f121.5, f124.5, ... in the 3-hourly part), so CDEPS
#                   interpolates them at the right time
#
# For every sfcf file:
#   - keep only the fields the field list needs, make time the record
#     (unlimited) dimension, and write 64-bit offset netCDF (readable
#     with PnetCDF; the sfcf files are netCDF-4)
#   - *_ave fields are 6-hour bucket averages (reset every FHZERO forecast
#     hours); they are converted to the mean over each output interval,
#     from this file and the one before it. Negative values from rounding
#     are set to 0 (all such fields here are downward fluxes or rates)
#   - f000: its *_ave fields (and those in F000_FILL_EXTRA) are not usable,
#     so they are taken from f001
#   - computed fields in the field list are added (g and R may be used)
# The mean stream also gets a record at the first and the last output hour
# (the first and last interval's means), so both streams cover the same
# period. Files are joined by valid date (UTC) into one file per day per
# stream, and <out-dir>/datm.streams describes both streams.
#
# Usage:
#   sfcf_to_datm.sh --cycle YYYYMMDDHH --fields datm_fields.txt --out-dir DIR \
#                   [--prefix gfsv17] [--data-path INPUT] \
#                   [--mesh INPUT/mesh.datm.3072x1536.nc] [--offset 0] \
#                   [--dtlimit X] sfcf000.nc sfcf001.nc ...
#   --prefix     daily file names: <prefix>_YYYYMMDD.nc and
#                <prefix>_mean_YYYYMMDD.nc (default gfsv17)
#   --data-path  directory the model run reads the daily files from, as
#                written in datm.streams (default INPUT)
#   --mesh       stream mesh file, as written in datm.streams
#                (default INPUT/mesh.datm.3072x1536.nc)
#   --offset     stream_offset in seconds, both streams (default 0)
#   --dtlimit    dtlimit for both streams (default, for each stream: 1.5 x
#                its largest gap between records / its smallest gap).
#                CDEPS stops if a gap exceeds dtlimit x the smallest; 1.0
#                fails on rounding even for equal gaps.
#   The sfcf files can be given in any order (a wildcard such as sfcf*.nc
#   is fine); the forecast hour is taken from the name (sfcfNNN.nc). They
#   must have no gaps that span a bucket reset (hourly or 3-hourly is fine).
#
# Field list, one line per DATM field, blank lines and # lines ignored:
#   name_in_file  name_in_model                 copy the sfcf field
#   name_in_file  name_in_model  expression     compute it (no spaces in it)
#
# Temporary files go in <out-dir>/tmp_sfcf_to_datm (about two days of
# output at most) and are removed at the end.
#=====================================================================

FHZERO=6                          # bucket length, hours (fhzero in input.nml)
G=9.80665                         # gravity (GFS con_g)
R=287.05                          # dry-air gas constant (GFS con_rd)
F000_FILL_EXTRA="dlwrf dswrf cpofp"   # instantaneous fields also taken from f001 at f000
STREAM_BASE="datm_forcing"        # stream names: datm_forcing_01, datm_forcing_02

PREFIX="gfsv17"
DATA_PATH="INPUT"
MESH="INPUT/mesh.datm.3072x1536.nc"
OFFSET=0
DTLIMIT=""                        # empty: from the spacing of each stream's records

usage() { sed -n '/^# Usage:/,/^# Temporary/p' "$0" | sed '$d; s/^# \{0,1\}//'; }
die() { echo "ERROR: $*"; exit 1; }

cycle=""; fields=""; outdir=""; files=()
while [ $# -gt 0 ]; do
  case "$1" in
    --cycle)     cycle=$2; shift ;;
    --fields)    fields=$2; shift ;;
    --out-dir)   outdir=$2; shift ;;
    --prefix)    PREFIX=$2; shift ;;
    --data-path) DATA_PATH=${2%/}; shift ;;
    --mesh)      MESH=$2; shift ;;
    --offset)    OFFSET=$2; shift ;;
    --dtlimit)   DTLIMIT=$2; shift ;;
    -h|--help)   usage; exit 0 ;;
    -*)          die "unknown option '$1' (see --help)" ;;
    *)           files+=("$1") ;;
  esac
  shift
done
[[ "${cycle}" =~ ^[0-9]{10}$ ]] || die "--cycle YYYYMMDDHH is required"
[ -f "${fields}" ]              || die "--fields: file '${fields}' not found"
[ -n "${outdir}" ]              || die "--out-dir is required"
[ ${#files[@]} -gt 0 ]          || die "no sfcf files given"
[[ "${OFFSET}" =~ ^-?[0-9]+$ ]] || die "--offset must be a whole number of seconds"
[ -z "${DTLIMIT}" ] || [[ "${DTLIMIT}" =~ ^[0-9]+(\.[0-9]+)?$ ]] || die "--dtlimit must be a positive number"
for t in ncks ncap2 ncrcat ncrename ncatted ncdump; do
  command -v ${t} > /dev/null || die "${t} not found (load the NCO module)"
done
cycle_str="${cycle:0:4}-${cycle:4:2}-${cycle:6:2} ${cycle:8:2}:00:00 UTC"
date -u -d "${cycle_str}" > /dev/null 2>&1 || die "--cycle ${cycle} is not a valid date"

#---------------------------------------------------------------------
# Field list
#---------------------------------------------------------------------
is_name() { [[ "$1" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; }
names_in() { grep -o '[A-Za-z_][A-Za-z0-9_]*' <<< "$1" | grep -v -x -E 'g|R|exp|log|sqrt|abs'; }
fname=(); mname=(); fexpr=(); fkind=()   # name in file, in model, expression ("" = copy), inst|mean
while read -r a b c rest; do
  [ -z "${a}" ] || [ "${a:0:1}" = "#" ] && continue
  [ -n "${b}" ] && [ -z "${rest}" ] || die "field list line '${a} ${b} ${c} ${rest}': need 2 or 3 words"
  is_name "${a}" || die "field list: '${a}' is not a valid name in the file"
  kind=inst
  for w in $( [ -n "${c}" ] && names_in "${c}" || echo "${a}" ); do
    [[ "${w}" == *_ave ]] && kind=mean
  done
  fname+=("${a}"); mname+=("${b}"); fexpr+=("${c}"); fkind+=("${kind}")
done < "${fields}"
[ ${#fname[@]} -gt 0 ] || die "no fields in ${fields}"

# sfcf fields to read: the copied ones, and the names used in expressions
need=()
add_need() { case " ${need[*]} " in *" $1 "*) ;; *) need+=("$1") ;; esac; }
for i in "${!fname[@]}"; do
  if [ -z "${fexpr[$i]}" ]; then
    add_need "${fname[$i]}"
  else
    for w in $(names_in "${fexpr[$i]}"); do add_need "${w}"; done
  fi
done
for i in "${!fname[@]}"; do
  [ -z "${fexpr[$i]}" ] && continue
  case " ${need[*]} " in
    *" ${fname[$i]} "*) die "field list: computed field '${fname[$i]}' has the name of an sfcf field that is used; choose another name" ;;
  esac
done
ave=()                                   # bucket-averaged fields among them
for w in "${need[@]}"; do [[ "${w}" == *_ave ]] && ave+=("${w}"); done
fill0=("${ave[@]}")                      # fields taken from f001 at f000
for w in ${F000_FILL_EXTRA}; do
  case " ${need[*]} " in *" ${w} "*) fill0+=("${w}") ;; esac
done
need_list=$(IFS=,; echo "${need[*]}")
inst_out=(); mean_out=()
for i in "${!fname[@]}"; do
  if [ "${fkind[$i]}" = "mean" ]; then mean_out+=("${fname[$i]}"); else inst_out+=("${fname[$i]}"); fi
done

#---------------------------------------------------------------------
# Files, in forecast-hour order
#---------------------------------------------------------------------
declare -A file_of
hours=()
for f in "${files[@]}"; do
  [ -f "${f}" ] || die "${f} not found"
  [[ "$(basename "${f}")" =~ f([0-9]{3,4})\.nc$ ]] || die "${f}: no forecast hour (fNNN.nc) in the name"
  h=$((10#${BASH_REMATCH[1]}))
  [ -z "${file_of[${h}]:-}" ] || die "two files for forecast hour ${h}"
  file_of[${h}]=${f}; hours+=("${h}")
done
mapfile -t hours < <(printf '%s\n' "${hours[@]}" | sort -n)
if [ "${hours[0]}" = "0" ] && [ "${#fill0[@]}" -gt 0 ] && [ -z "${file_of[1]:-}" ]; then
  die "f000 needs f001 (its averaged fields are taken from f001)"
fi

# The mean stream's times are shifted in the file's own time units: they must be hours
tunits=$(ncdump -h "${file_of[${hours[0]}]}" | sed -n 's/.*time:units = "\(.*\)".*/\1/p' | head -1)
[[ "${tunits}" == hours\ since* ]] || die "time units in ${file_of[${hours[0]}]} are '${tunits}', expected 'hours since ...'"

mkdir -p "${outdir}" || die "cannot make ${outdir}"
tmp="${outdir}/tmp_sfcf_to_datm"
rm -rf "${tmp}"; mkdir -p "${tmp}" || die "cannot make ${tmp}"

# Read one sfcf file: the needed fields, time as the record dimension, 64-bit offset
read_raw() {  # hour
  local out="${tmp}/raw_$1.nc"
  [ -f "${out}" ] && return 0
  ncks -O -6 --mk_rec_dmn time -v "${need_list}" "${file_of[$1]}" "${out}" \
    || die "reading ${file_of[$1]} (are all the fields in the list in it?)"
}

#---------------------------------------------------------------------
# One forecast hour: sfcf fields -> all forcing fields in work_<h>.nc
#---------------------------------------------------------------------
process_hour() {  # hour previous_hour (-1 for none)
  local h=$1 p=$2 w="${tmp}/work_$1.nc" v r expr i
  read_raw "${h}"
  cp "${tmp}/raw_${h}.nc" "${w}"

  if [ "${h}" = "0" ]; then
    # f000: take the averaged (and listed instantaneous) fields from f001
    read_raw 1
    ncks -A -C -v "$(IFS=,; echo "${fill0[*]}")" "${tmp}/raw_1.nc" "${w}" \
      || die "f000: filling from f001"
  elif [ ${#ave[@]} -gt 0 ]; then
    r=$(( FHZERO * ((h - 1) / FHZERO) ))     # last bucket reset before h
    if (( p >= 0 && p < r )); then
      die "f$(printf %03d ${h}): the previous output (f$(printf %03d ${p})) is before the bucket reset at f$(printf %03d ${r})"
    fi
    if (( p < 0 && h - r > 1 )); then
      echo " NOTE: f$(printf %03d ${h}) is the first file: its averaged fields are taken as the mean since f$(printf %03d ${r})"
    fi
    if (( p > r )); then
      # mean over (p, h] = (A_h * (h - r) - A_p * (p - r)) / (h - p)
      ncks -O -C -v "$(IFS=,; echo "${ave[*]}")" "${tmp}/raw_${p}.nc" "${tmp}/prev.nc" || die "f${p}: reading"
      for v in "${ave[@]}"; do ncrename -h -v "${v},${v}_prev" "${tmp}/prev.nc" > /dev/null || die "renaming ${v}"; done
      ncks -A -C "${tmp}/prev.nc" "${w}" || die "f${h}: adding f${p}"
      expr=""
      for v in "${ave[@]}"; do
        expr+="${v}=(${v}*$(( h - r ))-${v}_prev*$(( p - r )))/$(( h - p ));"
        expr+="where(${v}<0) ${v}=0;"
      done
      ncap2 -O -6 -s "${expr}" "${w}" "${w}" || die "f${h}: interval means"
      ncks -O -6 -x -v "$(IFS=,; for v in "${ave[@]}"; do printf '%s_prev,' "${v}"; done | sed 's/,$//')" "${w}" "${w}" \
        || die "f${h}: tidying"
    fi
  fi

  # Computed fields
  expr=""
  for i in "${!fname[@]}"; do
    [ -n "${fexpr[$i]}" ] && expr+="${fname[$i]}=${fexpr[$i]};"
  done
  if [ -n "${expr}" ]; then
    ncap2 -O -6 -s "g=${G}f;R=${R}f;${expr}" "${w}" "${w}" || die "f${h}: computing fields"
  fi
  # Keep only the forcing fields (and the coordinates)
  ncks -O -6 -v "$(IFS=,; echo "${fname[*]}")" "${w}" "${w}" || die "f${h}: selecting the forcing fields"
  # Attributes: computed fields describe themselves; interval means say so
  local att=()
  for i in "${!fname[@]}"; do
    [ -n "${fexpr[$i]}" ] && att+=(-a "long_name,${fname[$i]},o,c,${fexpr[$i]}" -a "cell_methods,${fname[$i]},d,,")
    [ "${fkind[$i]}" = "mean" ] && att+=(-a "cell_methods,${fname[$i]},o,c,time: mean (over the output interval; time is its middle)")
  done
  [ ${#att[@]} -gt 0 ] && { ncatted -O -h "${att[@]}" "${w}" || die "f${h}: attributes"; }
  return 0
}

# The fields of one stream from a work file, with the record's time shifted
stream_record() {  # work_file stream(inst|mean) shift_hours out_file
  local list
  if [ "$2" = "inst" ]; then list=$(IFS=,; echo "${inst_out[*]}"); else list=$(IFS=,; echo "${mean_out[*]}"); fi
  ncks -O -6 -v "${list}" "$1" "$4" || die "selecting the $2 fields"
  if [ "$3" != "0" ]; then
    ncap2 -O -6 -s "time=time+($3);" "$4" "$4" || die "setting the time of $4"
  fi
}

#---------------------------------------------------------------------
# Records, joined by valid date into one file per day per stream
#---------------------------------------------------------------------
declare -A cur_day day_list day_count
inst_names=(); mean_names=(); inst_times=(); mean_times=()
valid_day() { date -u -d "${cycle_str} + $(awk -v t="$1" 'BEGIN{printf "%d", t*60}') minutes" +%Y%m%d; }

flush_day() {  # stream
  local s=$1 name
  [ -n "${cur_day[$s]:-}" ] || return 0
  if [ "${s}" = "inst" ]; then name="${PREFIX}_${cur_day[$s]}.nc"; else name="${PREFIX}_mean_${cur_day[$s]}.nc"; fi
  # shellcheck disable=SC2086
  ncrcat -O -6 ${day_list[$s]} "${outdir}/${name}" || die "joining ${outdir}/${name}"
  rm -f ${day_list[$s]}
  if [ "${s}" = "inst" ]; then inst_names+=("${name}"); else mean_names+=("${name}"); fi
  echo " ${outdir}/${name}: ${day_count[$s]} times ($(du -h "${outdir}/${name}" | cut -f1))"
  day_list[$s]=""; day_count[$s]=0; cur_day[$s]=""
}

add_record() {  # stream file time_hours
  local s=$1 d
  d=$(valid_day "$3")
  [ -n "${cur_day[$s]:-}" ] && [ "${d}" != "${cur_day[$s]}" ] && flush_day "${s}"
  cur_day[$s]=${d}
  day_list[$s]="${day_list[$s]:-} $2"
  day_count[$s]=$(( ${day_count[$s]:-0} + 1 ))
  if [ "${s}" = "inst" ]; then inst_times+=("$3"); else mean_times+=("$3"); fi
}

echo " cycle ${cycle}, ${#hours[@]} files, f$(printf %03d ${hours[0]})-f$(printf %03d ${hours[-1]})"
echo " instantaneous (stream 01): ${inst_out[*]:-none}"
echo " interval means (stream 02): ${mean_out[*]:-none}"
[ "${hours[0]}" = "0" ] && echo " f000: ${fill0[*]} taken from f001"

prev=-1; last_mean=""
for h in "${hours[@]}"; do
  process_hour "${h}" "${prev}"
  w="${tmp}/work_${h}.nc"
  if [ ${#inst_out[@]} -gt 0 ]; then
    stream_record "${w}" inst 0 "${tmp}/inst_${h}.nc"
    add_record inst "${tmp}/inst_${h}.nc" "${h}"
  fi
  if [ ${#mean_out[@]} -gt 0 ]; then
    # time of the record: the middle of the interval the means cover
    if [ "${h}" = "0" ]; then
      half=0                                             # start record (f001's means)
    elif (( prev >= 0 )); then
      half=$(awk -v a="${prev}" -v b="${h}" 'BEGIN{print (b - a) / 2}')
    else
      half=$(awk -v h="${h}" -v z="${FHZERO}" 'BEGIN{r = z * int((h - 1) / z); print (h - r) / 2}')
    fi
    t=$(awk -v h="${h}" -v d="${half}" 'BEGIN{print h - d}')
    stream_record "${w}" mean "-${half}" "${tmp}/mean_${h}.nc"
    add_record mean "${tmp}/mean_${h}.nc" "${t}"
    last_mean="${tmp}/mean_${h}.nc"; last_half=${half}
  fi
  rm -f "${w}"
  # raw files no longer needed: everything before the previous hour (keep f001 for f000)
  if (( prev >= 0 )); then
    for f in "${tmp}"/raw_*.nc; do
      [ -e "${f}" ] || continue
      k=${f##*raw_}; k=${k%.nc}
      (( k < prev )) && [ "${k}" != "1" ] && rm -f "${f}"
    done
  fi
  prev=${h}
done
# End record for the mean stream: the last interval's means at the last output hour
if [ -n "${last_mean}" ] && [ "${hours[-1]}" != "0" ]; then
  cp "${last_mean}" "${tmp}/mean_end.nc"
  ncap2 -O -6 -s "time=time+(${last_half});" "${tmp}/mean_end.nc" "${tmp}/mean_end.nc" || die "end record"
  add_record mean "${tmp}/mean_end.nc" "${hours[-1]}"
fi
flush_day inst
flush_day mean
rm -rf "${tmp}"

#---------------------------------------------------------------------
# datm.streams (ESMF config): stream 01 instantaneous, stream 02 means
#---------------------------------------------------------------------
dt_limit() {  # times...: 1.5 x largest gap / smallest gap (or --dtlimit)
  [ -n "${DTLIMIT}" ] && { echo "${DTLIMIT}"; return; }
  printf '%s\n' "$@" | awk 'NR > 1 { d = $1 - p; if (mn == "" || d < mn) mn = d; if (d > mx) mx = d }
                            { p = $1 }
                            END { if (mn == "" || mn <= 0) print "1.5"; else printf "%.1f\n", 1.5 * mx / mn }'
}
syear=${cycle:0:4}
lyear=$(date -u -d "${cycle_str} + ${hours[-1]} hours" +%Y)

stream_block() {  # id kind
  local id=$1 kind=$2 n data_files="" variables="" vectors="null" dtl i
  if [ "${kind}" = "inst" ]; then
    for n in "${inst_names[@]}"; do data_files+=" \"${DATA_PATH}/${n}\""; done
    dtl=$(dt_limit "${inst_times[@]}")
  else
    for n in "${mean_names[@]}"; do data_files+=" \"${DATA_PATH}/${n}\""; done
    dtl=$(dt_limit "${mean_times[@]}")
  fi
  for i in "${!fname[@]}"; do
    [ "${fkind[$i]}" = "${kind}" ] && variables+=" \"${fname[$i]} ${mname[$i]}\""
  done
  case " ${variables} " in *'"'*" Sa_u\""*) case " ${variables} " in *'"'*" Sa_v\""*) vectors='"Sa_u:Sa_v"' ;; esac ;; esac
  cat <<EOF
taxmode${id}:                 limit
mapalgo${id}:                 bilinear
tInterpAlgo${id}:             linear
readMode${id}:                single
dtlimit${id}:                 ${dtl}
stream_offset${id}:           ${OFFSET}
yearFirst${id}:               ${syear}
yearLast${id}:                ${lyear}
yearAlign${id}:               ${syear}
stream_vectors${id}:          ${vectors}
stream_mesh_file${id}:        "${MESH}"
stream_lev_dimname${id}:      null
stream_data_files${id}:      ${data_files}
stream_data_variables${id}:  ${variables}
EOF
}

ids=(); kinds=()
[ ${#inst_out[@]} -gt 0 ] && { ids+=("$(printf %02d $(( ${#ids[@]} + 1 )))"); kinds+=(inst); }
[ ${#mean_out[@]} -gt 0 ] && { ids+=("$(printf %02d $(( ${#ids[@]} + 1 )))"); kinds+=(mean); }
{
  printf 'stream_info:               '
  for id in "${ids[@]}"; do printf '%s_%s ' "${STREAM_BASE}" "${id}"; done | sed 's/ $//'
  echo
  for k in "${!ids[@]}"; do
    echo
    stream_block "${ids[$k]}" "${kinds[$k]}"
  done
} > "${outdir}/datm.streams"
for k in "${!ids[@]}"; do
  echo " ${outdir}/datm.streams: stream ${STREAM_BASE}_${ids[$k]} (${kinds[$k]}): dtlimit $(grep "^dtlimit${ids[$k]}:" "${outdir}/datm.streams" | awk '{print $2}')"
done
echo " done"
