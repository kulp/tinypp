/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "libtcc.c"

#include <string.h>

static int parse_args(TCCState *s, int argc, char **argv)
{
    int i;
    char *optarg;
    for (i = 0, optarg = argv[i]; i < argc; optarg = argv[++i]) {
        if (!strncmp(optarg, "-D", 2)) {
            char *sym = optarg + 2, *value;
            value = strchr(sym, '=');
            if (value)
                *value++ = '\0';
            tcc_define_symbol(s, sym, value);
        } else if (!strncmp(optarg, "-U", 2)) {
            tcc_undefine_symbol(s, optarg + 2);
        } else if (!strncmp(optarg, "-I", 2)) {
            tcc_add_include_path(s, optarg + 2);
        }
    }

    return 0;
}

// only acts as a pipe
int main(int argc, char **argv)
{
    TCCState *s = tcc_new();
    s->outfile = stdout;

    tcc_set_output_type(s, TCC_OUTPUT_PREPROCESS);
    parse_args(s, argc - 1, argv + 1);

    return tcc_add_file(s, "-");
}

