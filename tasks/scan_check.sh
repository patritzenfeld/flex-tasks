#!/bin/bash
# Usage: ./analyze_code.sh <file_to_analyze>
FILE="Check.hs"
MAX_LENGTH=200
MIN_CLONE=80
REPORT="scan_check.html"

# Clear any existing report.
> "$REPORT"

echo "<style>" \
  "body {background-color: aliceblue;}" \
  "p {text-indent:2em;}" \
  ".problem {color: red;}" \
  ".clone {color: darkred;}" \
  ".header {margin: 2em 0; color: darkorange; font-size: 1.5em;}" \
"</style>" >> $REPORT

echo "<body>" >> $REPORT
awk -v max="$MAX_LENGTH" -v min_clone="$MIN_CLONE" -v report="$REPORT" '
{
    line = $0
    L = length(line)
    # Save each line for later extension.
    lines[NR] = line

    # Accumulate long line messages.
    if (L > max) {
        long_output = long_output sprintf("<pre class=problem>Line %d: %d characters long.</pre>", NR, L)
    }

    # For each possible substring of length min_clone in the line...
    for (i = 1; i <= L - min_clone + 1; i++) {
        sub_str = substr(line, i, min_clone)
        # Skip substrings made up solely of whitespace.
        if (sub_str ~ /^[ \t]+$/)
            continue
        occ = NR ":" i
        if (sub_str in seen) {
            seen[sub_str] = seen[sub_str] ", " occ
            count[sub_str]++
        } else {
            seen[sub_str] = occ
            count[sub_str] = 1
        }
    }
}
END {
    # For each duplicate fixed substring candidate...
    for (s in count) {
        if (count[s] > 1) {
            # Split the occurrence list (format: line:offset, line:offset, ...)
            n = split(seen[s], occ_arr, ", ")
            # Use the first occurrence as the base.
            split(occ_arr[1], parts, ":")
            base_line = parts[1]
            base_offset = parts[2] + 0

            # Check backward extension.
            base_prev = (base_offset > 1) ? substr(lines[base_line], base_offset - 1, 1) : ""
            extendable_back = 1
            for (k = 1; k <= n; k++) {
                split(occ_arr[k], parts, ":")
                cur_line = parts[1]
                cur_offset = parts[2] + 0
                cur_prev = (cur_offset > 1) ? substr(lines[cur_line], cur_offset - 1, 1) : ""
                if (cur_prev != base_prev) {
                    extendable_back = 0
                    break
                }
            }
            if (base_prev != "" && extendable_back == 1)
                continue

            # Extend the match forward across all occurrences.
            ext = 0
            while (1) {
                pos = base_offset + min_clone + ext
                if (pos > length(lines[base_line]))
                    break
                ext_char = substr(lines[base_line], pos, 1)
                match_all = 1
                for (k = 2; k <= n; k++) {
                    split(occ_arr[k], parts, ":")
                    cur_line = parts[1]
                    cur_offset = parts[2] + 0
                    pos2 = cur_offset + min_clone + ext
                    if (pos2 > length(lines[cur_line])) {
                        match_all = 0
                        break
                    }
                    if (substr(lines[cur_line], pos2, 1) != ext_char) {
                        match_all = 0
                        break
                    }
                }
                if (match_all == 1) {
                    ext++
                } else {
                    break
                }
            }
            clone_length = min_clone + ext
            clone_str = substr(lines[base_line], base_offset, clone_length)

            # Build a unique key based on the set of affected lines.
            n_occ = split(seen[s], occ_arr, ", ")
            delete lineNumbers
            lineKey = ""
            for (k = 1; k <= n_occ; k++) {
                split(occ_arr[k], parts, ":")
                ln = parts[1]
                if (!(ln in lineNumbers)) {
                    lineNumbers[ln] = 1
                    lineKey = (lineKey == "" ? ln : lineKey "," ln)
                }
            }
            # Record the candidate for this set of lines, keeping the longest clone.
            if (!(lineKey in bestClone) || clone_length > bestClone[lineKey]) {
                bestClone[lineKey] = clone_length
                bestCloneStr[lineKey] = clone_str
                bestClonePos[lineKey] = lineKey
            }
        }
    }
    
    # Second pass: if a candidate'\''s set of lines is a subset of another'\''s,
    # drop the one with fewer lines.
    for (k1 in bestClone) {
        n1 = split(bestClonePos[k1], arr1, ",")
        for (k2 in bestClone) {
            if (k1 == k2)
                continue
            n2 = split(bestClonePos[k2], arr2, ",")
            if (n1 < n2) {
                subset = 1
                for (i = 1; i <= n1; i++) {
                    found = 0
                    for (j = 1; j <= n2; j++) {
                        if (arr1[i] == arr2[j]) {
                            found = 1
                            break
                        }
                    }
                    if (!found) {
                        subset = 0
                        break
                    }
                }
                if (subset == 1)
                    toRemove[k1] = 1
            }
        }
    }
    
    # Accumulate clone output.
    for (lk in bestClone) {
        if (!(lk in toRemove)) {
            clone_output = clone_output sprintf("<pre class=problem>Duplicate (length %d) at lines %s:</pre> <p class=clone>%s</p>" \
                                               , bestClone[lk], bestClonePos[lk], bestCloneStr[lk])
        }
    }
    
    # Output section headings only if their respective messages exist.
    if (long_output != "") {
        printf (\
          "<pre class=header>The following lines are longer than %d characters. " \
          "It is likely they are large interpolated data structures.\n" \
          "Consider one of the following approaches:\n" \
          "  1. Evaluate them before interpolation\n" \
          "  2. Directly define the interpolated value inside the Check module instead of interpolating it.\n" \
          "  3. Supply them via the saved TaskData instead.</pre>",
          max) >> report
        print long_output >> report
    }
    if (clone_output != "") {
        printf (\
          "<pre class=header>The following lines contain exact duplicates longer than %d characters. " \
          "It is likely they are partly identical interpolated values.\n" \
          "Consider locally defining them instead, then use the local definition: myDef = #{whatToInterpolate}</pre>",
          min_clone) >> report
        print clone_output >> report
    }
}
' "$FILE"

echo "</body>" >> $REPORT

