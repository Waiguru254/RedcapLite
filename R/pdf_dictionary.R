create_form_table <- function(data) {
  # CSS styles for the table
  css_styles <- "
  <style>
    table {
      border-collapse: collapse;
      width: 100%;
      font-family: Arial, sans-serif;
      margin: 20px 0;
      border-top: 3px solid #000;    /* Bold upper border */
      border-bottom: 3px solid #000; /* Bold lower border */
    }
    th, td {
      border: 1.5px solid #000;  /* Bold border between cells */
      padding: 8px;
      text-align: left;
    }
    th {
      background-color: #f2f2f2;
    }
    /* Make second column (Label) bold */
    td:nth-child(2) {
      font-weight: bold;
    }
    tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    thead tr {
      border-bottom: 3px solid #000; /* Bold border below header */
    }
    tbody tr:last-child {
      border-bottom: none; /* Remove default border for last row */
    }
    .checkbox:before {
      content: '☐';
      margin-right: 5px;
    }
    .radio:before {
      content: '○';
      margin-right: 5px;
    }
    .dropdown:before {
      content: '▼';
      margin-right: 5px;
      font-size: 0.8em;
    }
    input[type='text'],
    input[type='date'],
    input[type='number'] {
      width: 200px;
      padding: 5px;
      border: 1px solid #ddd;
      border-radius: 3px;
    }
    select {
      width: 200px;
      padding: 5px;
      border: 1px solid #ddd;
      border-radius: 3px;
    }
  </style>"

  # Function to format options based on type
  format_options <- function(options, type) {
    # Handle text, date, and other input types
    if (type %in% c("text", "date")) {
      input_type <- if(type == "date") "date" else "text"
      return(sprintf('<input type="%s" placeholder="Enter Date">', input_type))
    }

    if (is.na(options) || options == "NA" || options == "") {
      return('<input type="text" placeholder="Enter value">')
    }

    # Split options and clean them
    options_list <- str_split(options, "\\|")[[1]]
    options_list <- str_trim(options_list)
    options_list <- str_replace_all(options_list, "^\\d+,\\s*", "")  # Remove leading numbers

    # Format based on type
    formatted_options <- switch(type,
                                "checkbox" = paste(sprintf('<div class="checkbox">%s</div>', options_list), collapse = "\n"),
                                "radio" = paste(sprintf('<div class="radio">%s</div>', options_list), collapse = "\n"),
                                "dropdown" = {
                                  select_html <- '<select>\n'
                                  select_html <- paste0(select_html, '<option value="">Select an option</option>\n')
                                  select_html <- paste0(select_html,
                                                        paste(sprintf('<option value="%s">%s</option>',
                                                                      options_list, options_list),
                                                              collapse = "\n"))
                                  paste0(select_html, '</select>')
                                },
                                "yesno" = paste(sprintf('<div class="radio">%s</div>', c("Yes", "No")), collapse = "\n"),
                                sprintf('<input type="text" placeholder="Enter %s">', type)  # Default case
    )

    return(formatted_options)
  }

  # Create table header
  table_html <- paste0(css_styles, '
  <table>
    <thead>
      <tr>
        <th>Field Name</th>
        <th>Label</th>
        <th>Options</th>
      </tr>
    </thead>
    <tbody>')

  # Process each row
  for (i in 1:nrow(data)) {
    row <- data[i, ]

    # Format the row
    table_row <- sprintf('
      <tr>
        <td>%s</td>
        <td>%s</td>
        <td>%s</td>
      </tr>',
                         row$field_name,
                         row$field_label,
                         format_options(row$select_choices_or_calculations, row$field_type)
    )

    table_html <- paste0(table_html, table_row)
  }

  # Close table
  table_html <- paste0(table_html, '
    </tbody>
  </table>')

  return(table_html)
}
# # Generate HTML table
#  html_output <- create_form_table(dict)
# # Write to file or display in R viewer
#  html_content <- htmltools::HTML(html_output)
#  htmltools::browsable(html_content)
#  write(html_output, "form_table.html")
#  # Convert HTML to PDF
# # webshot2::webshot("form_table.html", "table.pdf")
#   # Open the PDF
# #browseURL("table.pdf")
