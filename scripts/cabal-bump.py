import sys
import os
import re
import json

def main():
    if len(sys.argv) < 3:
        print("Usage: python3 bump_version.py <comma_separated_files> <json_labels_array>")
        sys.exit(1)

    files_str = sys.argv[1]
    labels_str = sys.argv[2]

    try:
        labels = json.loads(labels_str)
    except json.JSONDecodeError:
        # Fallback if the user passes a simple string instead of JSON array
        labels = [labels_str]

    bump_type = None
    if "6. release: major" in labels: bump_type = "major"
    elif "6. release: minor" in labels: bump_type = "minor"
    elif "6. release: patch" in labels: bump_type = "patch"

    if not bump_type:
        print("No release label found. Skipping version bump.")
        sys.exit(0)

    print(f"Detected bump type: {bump_type}")

    changed_files = files_str.split(",")
    cabal_files_to_update = set()

    for file_path in changed_files:
        parts = file_path.split("/")
        if len(parts) > 1:
            root_dir = parts[0]
            if os.path.isdir(root_dir):
                for f in os.listdir(root_dir):
                    if f.endswith(".cabal"):
                        cabal_files_to_update.add(os.path.join(root_dir, f))

    if not cabal_files_to_update:
        print("No cabal files found corresponding to changed files.")
        sys.exit(0)

    version_regex = re.compile(r"^(version:\s+)(\d+)\.(\d+)\.(\d+)(.*)$")

    for cabal_file in cabal_files_to_update:
        print(f"Updating {cabal_file}...")

        try:
            with open(cabal_file, "r") as f:
                lines = f.readlines()

            new_lines = []
            updated = False

            for line in lines:
                match = version_regex.match(line)
                if match and not updated:
                    prefix, major, minor, patch, suffix = match.groups()
                    major, minor, patch = int(major), int(minor), int(patch)

                    if bump_type == "major":
                        major += 1
                        minor = 0
                        patch = 0
                    elif bump_type == "minor":
                        minor += 1
                        patch = 0
                    elif bump_type == "patch":
                        patch += 1

                    new_line = f"{prefix}{major}.{minor}.{patch}{suffix}\n"
                    new_lines.append(new_line)
                    updated = True
                else:
                    new_lines.append(line)

            with open(cabal_file, "w") as f:
                f.writelines(new_lines)

        except FileNotFoundError:
            print(f"Error: Could not find file {cabal_file}")

if __name__ == "__main__":
    main()
