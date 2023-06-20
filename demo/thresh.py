atom0_aa = [None, None]
atom0_abx = [None, None]
atom0_b = [None, None]
i_0 = 0
atom1_a = [None]
atom1_b = [None]
i_1 = 0
def threshold_ltl_0(meter_va, is_breaker_open, thresh):
    global atom0_aa, atom0_abx, atom0_b, i_0
    j = i_0
    if i_0 == 2:
        atom0_aa = atom0_aa[1:] + atom0_aa[:1]
        atom0_abx = atom0_abx[1:] + atom0_abx[:1]
        atom0_b = atom0_b[1:] + atom0_b[:1]
        j = 1
    atom0_aa[j] = not (not is_breaker_open and meter_va > thresh)
    atom0_abx[j] = is_breaker_open
    atom0_b[j] = not (True)
    if (atom0_aa[0] is not None) and (atom0_abx[1] is not None) and (atom0_b[0] is not None):
        if atom0_aa[0] or atom0_abx[1]:
            print("Looks good.")
        else:
            print("Monitor failed!")
    else:
        print("Buffering...")
    if i_0 < 2:
        i_0 = i_0 + 1
def threshold_ltl_1(meter_va, is_breaker_open, thresh):
    global atom1_a, atom1_b, i_1
    j = i_1
    if i_1 == 1:
        atom1_a = atom1_a[1:] + atom1_a[:1]
        atom1_b = atom1_b[1:] + atom1_b[:1]
        j = 0
    atom1_a[j] = meter_va > 0
    atom1_b[j] = not (True)
    if (atom1_a[0] is not None) and (atom1_b[0] is not None):
        if atom1_a[0]:
            print("Looks good.")
        else:
            print("Monitor failed!")
    else:
        print("Buffering...")
    if i_1 < 1:
        i_1 = i_1 + 1
def threshold(meter_va, is_breaker_open, thresh):
    if not is_breaker_open and meter_va > thresh:
        return True
    else:
        return False
