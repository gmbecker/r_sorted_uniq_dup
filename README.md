# r_sorted_uniq_dup

Patch for internal (ie default) unique/duplicated to to use a 100% backwards-compatible, 
much more efficient algorithm (well, "algorithm") when the input is an ALTREP which knows 
is sorted.

Comments welcome. 

To use this patch locally, set up a checkout of the R sources you would be able to build 
normally, then copy the contents of src/ in this repo into that source code over-riding 
the R source-code files. Then build as you normally would.

I typically do the R configure with `--prefix=<home_dir>/local/<name_related_to_patch>` 
to keep things straight.
