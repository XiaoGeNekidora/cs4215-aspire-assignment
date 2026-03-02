import os
import sys
import subprocess

def run_tests(folder_path):
    # Ensure the folder exists
    if not os.path.exists(folder_path):
        print(f"Error: Folder '{folder_path}' does not exist.")
        sys.exit(1)

    # Get all files in the directory recursively
    files = []
    for root, dirs, filenames in os.walk(folder_path):
        for filename in filenames:
            full_path = os.path.join(root, filename)
            files.append(full_path)
    
    # Sort files to ensure consistent run order
    files.sort()

    passed = 0
    failed = 0
    total = 0

    print(f"Running tests in: {folder_path}\n" + "="*40)

    for file_path in files:
        # Get a relative name for display
        display_name = os.path.relpath(file_path, folder_path)
        
        # Read the content of the file
        with open(file_path, 'r') as f:
            file_content = f.read()

        try:
            # Run the command with the file content as stdin
            # Command: dune exec -- aspire eval-cmp
            process = subprocess.run(
                ["dune", "exec", "--", "aspire", "eval-cmp"],
                input=file_content,
                text=True,           # Treat input/output as string (not bytes)
                capture_output=True, # Capture stdout and stderr
                check=False          # Don't raise exception on non-zero exit code
            )

            # Check the output
            output = process.stdout.strip()
            
            # You might want to also check stderr if errors are printed there
            error_output = process.stderr.strip()

            if output == "Outputs are identical":
                print(f"[PASS] {display_name}")
                passed += 1
            else:
                print(f"[FAIL] {display_name}")
                print(f"   Expected: Outputs are identical")
                print(f"   Got     : {output if output else '<empty>'}")
                if error_output:
                    print(f"   Stderr  : {error_output}")
                failed += 1

        except Exception as e:
            print(f"[ERR ] {display_name} - Exception occurred: {e}")
            failed += 1
        
        total += 1

    print("="*40)
    print(f"Total: {total}, Passed: {passed}, Failed: {failed}")

    if failed > 0:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test_runner.py <path_to_test_folder>")
        print("Example: python3 test_runner.py test")
        sys.exit(1)

    target_folder = sys.argv[1]
    run_tests(target_folder)