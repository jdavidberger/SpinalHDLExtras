suffix=".v"  # Replace with the suffix you want to remove
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

for file in *"$suffix"; do
  if [ -f "$file" ]; then  # Check if it's a regular file
    new_name="${file%$suffix}"  # Remove the suffix from the filename
    export FILE=${new_name}
    yosys -c ${SCRIPT_DIR}/flatten.tcl
  fi
done