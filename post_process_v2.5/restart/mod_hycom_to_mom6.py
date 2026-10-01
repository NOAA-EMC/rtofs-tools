import numpy as np

def h2m_p(f_nc, nto, mto, misval=np.nan, spval=0.0):
    """
    Convert p-grid HYCOM array to MOM6 grid.
    f_nc shape: (kk, jj, ii) -> Returns: (kk, mto, nto)
    """
    kk, jj, ii = f_nc.shape
    field = np.zeros((kk, mto, nto), dtype=f_nc.dtype)
    
    # Map valid bounds
    valid_y = min(mto, jj)
    valid_x = min(nto, ii)
    
    # Direct copy
    field[:, :valid_y, :valid_x] = f_nc[:, :valid_y, :valid_x]
    
    # Replace missing values with spval
    if np.isnan(misval):
        field[np.isnan(field)] = spval
    else:
        field[field == misval] = spval
        
    return field

def h2m_u(f_nc, ntq, mto, lsymetr, misval=np.nan, spval=0.0):
    """
    Convert u-grid HYCOM array to MOM6 grid.
    MOM6 symmetric has "q" at i-0.5,j-0.5 w.r.t p.ij (direct mapping with wrap).
    MOM6 standard has "q" at i+0.5,j+0.5 w.r.t  p.ij (shifted mapping).
    HYCOM         has "q" at i-0.5,j-0.5 w.r.t. p.ij
    """
    kk, jj, ii = f_nc.shape
    field = np.zeros((kk, mto, ntq), dtype=f_nc.dtype)
    
    valid_y = min(mto, jj)
    
    if lsymetr:
        # Direct mapping up to min(ntq, ii)
        valid_x = min(ntq, ii)
        field[:, :valid_y, :valid_x] = f_nc[:, :valid_y, :valid_x]
        
        # Periodic wrap if ntq > ii
        if ntq > ii:
            wrap_len = ntq - ii
            field[:, :valid_y, ii:ntq] = f_nc[:, :valid_y, :wrap_len]
    else:
        # Non-symmetric shifts data left by 1 with wraparound.
        # Fortran: ia = mod(i, ntq) + 1  -> Python: np.roll(shift=-1)
        valid_x = min(ntq, ii)
        f_slice = f_nc[:, :valid_y, :valid_x]
        field[:, :valid_y, :valid_x] = np.roll(f_slice, shift=-1, axis=2)
        
    # Replace missing values with spval
    if np.isnan(misval):
        field[np.isnan(field)] = spval
    else:
        field[field == misval] = spval
        
    return field

def h2m_v(f_nc, nto, mtq, lsymetr, misval=np.nan, spval=0.0):
    """
    Convert v-grid HYCOM array to MOM6 grid.
    MOM6 symmetric has "q"  at i-0.5,j-0.5 w.r.t p.ij (direct mapping).
    MOM6 standard  has "q"  at i+0.5,j+0.5 w.r.t p.ij (shifted mapping).
    HYCOM          has "q"  at i-0.5,j-0.5 w.r.t p.ij
    """
    kk, jj, ii = f_nc.shape
    field = np.zeros((kk, mtq, nto), dtype=f_nc.dtype)
    
    valid_x = min(nto, ii)
    
    if lsymetr:
        # Direct mapping
        valid_y = min(mtq, jj)
        field[:, :valid_y, :valid_x] = f_nc[:, :valid_y, :valid_x]
    else:
        # Non-symmetric shifts data "down" by 1 row in the y-axis
        valid_y = min(mtq, jj - 1)
        
        # Fortran: field(i, j, k) = f_nc(i, j+1, k)
        field[:, :valid_y, :valid_x] = f_nc[:, 1:valid_y+1, :valid_x]
        
        # Fortran: field(i, 1, k) = 0.0 (Python 0-index: row 0)
        field[:, 0, :valid_x] = 0.0
        
    # Replace missing values with spval
    if np.isnan(misval):
        field[np.isnan(field)] = spval
    else:
        field[field == misval] = spval
        
    return field
