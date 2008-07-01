/* moving data in small power-of-2 sized units */
void copy_el(char *dest, char *src, size_t sz)
{
    switch (sz) {
    case 16:
        *(int64_t*)&dest[0] = *(int64_t*)&src[0];
        *(int64_t*)&dest[8] = *(int64_t*)&src[8];
        break;
    case 8: *(int64_t*)dest = *(int64_t*)src; break;
    case 4: *(int32_t*)dest = *(int32_t*)src; break;
    case 2: *(int16_t*)dest = *(int16_t*)src; break;
    case 1: *dest = *src; break;
    }
}

void swap_el(char *a, char *b, size_t sz)
{
    int64_t i64;
    int32_t i32;
    int16_t i16;
    int8_t i8;
    switch (sz) {
    case 16:
        i64 = *(int64_t*)&a[0];
        *(int64_t*)&a[0] = *(int64_t*)&b[0];
        *(int64_t*)&b[0] = i64;
        i64 = *(int64_t*)&a[8];
        *(int64_t*)&a[8] = *(int64_t*)&b[8];
        *(int64_t*)&b[8] = i64;
        break;
    case 8:
        i64 = *(int64_t*)a;
        *(int64_t*)a = *(int64_t*)b;
        *(int64_t*)b = i64;
        break;
    case 4:
        i32 = *(int32_t*)a;
        *(int32_t*)a = *(int32_t*)b;
        *(int32_t*)b = i32;
        break;
    case 2:
        i16 = *(int16_t*)a;
        *(int16_t*)a = *(int16_t*)b;
        *(int16_t*)b = i16;
        break;
    case 1:
        i8 = *a;
        *a = *b;
        *b = i8;
        break;
    }
}

void neg_any(void *dest, void *a, numerictype_t tag)
{
    switch (tag) {
    case T_INT8:   *(int8_t  *)dest = -*(int8_t  *)a; break;
    case T_UINT8:  *(uint8_t *)dest = -*(uint8_t *)a; break;
    case T_INT16:  *(int16_t *)dest = -*(int16_t *)a; break;
    case T_UINT16: *(uint16_t*)dest = -*(uint16_t*)a; break;
    case T_INT32:  *(int32_t *)dest = -*(int32_t *)a; break;
    case T_UINT32: *(uint32_t*)dest = -*(uint32_t*)a; break;
    case T_INT64:  *(int64_t *)dest = -*(int64_t *)a; break;
    case T_UINT64: *(uint64_t*)dest = -*(uint64_t*)a; break;
    case T_FLOAT:  *(float   *)dest = -*(float   *)a; break;
    case T_DOUBLE: *(double  *)dest = -*(double  *)a; break;
    }
}
