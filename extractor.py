import os
import re
import sys
import argparse

def get_next_id(output_dir):
    """
    Scans the output directory for files named 'N.aspire'.
    Returns the next available integer ID.
    """
    max_id = 0
    if not os.path.exists(output_dir):
        return 1

    for filename in os.listdir(output_dir):
        if filename.endswith(".aspire"):
            name_part = os.path.splitext(filename)[0]
            if name_part.isdigit():
                num = int(name_part)
                if num > max_id:
                    max_id = num
    return max_id + 1

def extract_tests(md_file_path, output_dir):
    # Create directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created directory: {output_dir}")

    try:
        with open(md_file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: File '{md_file_path}' not found.")
        return

    # Regex to capture content inside ```scheme ... ``` blocks
    # re.DOTALL makes '.' match newlines as well
    pattern = r"```scheme\s*(.*?)\s*```"
    matches = re.findall(pattern, content, re.DOTALL)

    if not matches:
        print("No ```scheme``` code blocks found in the file.")
        return

    current_id = get_next_id(output_dir)
    count = 0

    for match in matches:
        code_content = match.strip()
        
        # Skip empty blocks if any
        if not code_content:
            continue

        filename = f"{current_id}.aspire"
        file_path = os.path.join(output_dir, filename)

        with open(file_path, 'w', encoding='utf-8') as out_file:
            out_file.write(code_content + "\n")
        
        print(f"Extracted test to: {file_path}")
        
        current_id += 1
        count += 1

    print(f"\nSuccessfully extracted {count} test cases.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract Aspire test cases from a Markdown file.")
    parser.add_argument("output_dir", help="Directory to save the .aspire files")
    parser.add_argument("md_file", help="Input Markdown file containing test cases")
    
    args = parser.parse_args()
    
    extract_tests(args.md_file, args.output_dir)