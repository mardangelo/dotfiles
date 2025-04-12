

import base64
import os

# --- CSS Templates ---
CSS_TEMPLATE_REGULAR = """\
@font-face {{
    font-family: "{name}";
    font-style: normal;
    font-weight: 400;
    src: url(data:font/{extension};charset=utf-8;base64,{b64_data});
}}
"""
CSS_TEMPLATE_BOLD = """\
@font-face {{
    font-family: "{name}";
    font-style: normal;
    font-weight: 700;
    src: url(data:font/{extension};charset=utf-8;base64,{b64_data});
}}
"""
CSS_TEMPLATE_ITALIC = """\
@font-face {{
    font-family: "{name}";
    font-style: italic;
    font-weight: 400;
    src: url(data:font/{extension};charset=utf-8;base64,{b64_data});
}}
"""
CSS_TEMPLATE_BOLD_ITALIC = """\
@font-face {{
    font-family: "{name}";
    font-style: italic;
    font-weight: 700;
    src: url(data:font/{extension};charset=utf-8;base64,{b64_data});
}}
"""

# --- Configuration ---
STYLES_TO_PROCESS = ["Regular", "Bold", "Italic", "BoldItalic"]
FONT_EXTENSION = "ttf"

TEMPLATE_MAP = {
    "Regular": CSS_TEMPLATE_REGULAR,
    "Bold": CSS_TEMPLATE_BOLD,
    "Italic": CSS_TEMPLATE_ITALIC,
    "BoldItalic": CSS_TEMPLATE_BOLD_ITALIC,
}

# --- Main Function ---
def generate_font_css(base_font_name):
    """
    Generates CSS @font-face rules for different styles of a font.

    Args:
        base_font_name (str): The base name of the font family (e.g., "Roboto").

    Returns:
        str: A string containing all the generated CSS rules, or an empty
             string if no files were processed successfully.
    """
    all_css_rules = []

    print(f"Generating CSS for font family: '{base_font_name}'")
    print("-" * 30)

    for style in STYLES_TO_PROCESS:
        # Construct filename: FontName-StyleName.ttf (remove spaces from style)
        style_filename_part = style.replace(" ", "")
        file_path = f"{base_font_name}-{style_filename_part}.{FONT_EXTENSION}"

        print(f"Processing Style: '{style}'")
        print(f" -> Looking for file: '{file_path}'")

        try:
            # Read font file in binary mode
            with open(file_path, "rb") as font_file:
                font_data = font_file.read()

            # Base64 encode the binary data and decode to UTF-8 string
            b64_encoded_data = base64.b64encode(font_data).decode("utf-8")

            # Get the appropriate template
            template = TEMPLATE_MAP.get(style)
            if not template:
                print(f" -> Warning: No template found for style '{style}'. Skipping.")
                continue 

            # Format the CSS rule
            css_rule = template.format(
                name=base_font_name,
                extension=FONT_EXTENSION,
                b64_data=b64_encoded_data
            )
            all_css_rules.append(css_rule)
            print(f" -> Successfully generated CSS for '{style}'.")

        except FileNotFoundError:
            print(f" -> Warning: File not found: '{file_path}'. Skipping this style.")
        except Exception as e:
            print(f" -> Error processing file '{file_path}': {e}")

        print("-" * 10) # Separator for styles

    return "\n".join(all_css_rules)

# --- Execution ---
if __name__ == "__main__":
    font_name_input = input("Enter the base font name (e.g., 'Arial', 'MyCustomFont'): ")

    if not font_name_input:
        print("Error: No font name provided.")
        sys.exit(1)

    # Clean up the input name for use in filenames etc.
    clean_font_name = font_name_input.strip()

    generated_css = generate_font_css(clean_font_name)

    if generated_css:
        # --- Write to file instead of printing ---
        output_filename = f"{clean_font_name}-fontface.css"
        print(f"\nAttempting to save CSS to '{output_filename}'...")
        try:
            # Use 'with' statement for safe file handling
            # Specify encoding='utf-8' which is standard for CSS
            with open(output_filename, "w", encoding="utf-8") as f:
                f.write(generated_css)
            print(f"Successfully saved CSS to '{output_filename}'")
        except IOError as e:
            print(f"\nError: Could not write CSS to file '{output_filename}'.")
            print(f"Reason: {e}")
            # Optionally print the CSS to console as a fallback
            # print("\n--- Generated CSS (Failed to Save) ---")
            # print(generated_css)
        except Exception as e:
             print(f"\nAn unexpected error occurred while saving the file: {e}")

    else:
        print("\nNo CSS rules were generated. Please check file paths and names.")
        print("No output file created.")
