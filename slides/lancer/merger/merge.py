from PyPDF2 import PdfReader, PdfWriter
from PyPDF2 import PageObject
from PyPDF2.constants import PageAttributes as PA

def resize_page(page, new_width, new_height):
    """ Resize a PDF page to new dimensions """
    new_page = PageObject.create_blank_page(width=new_width, height=new_height)
    new_page.mergeScaledPage(page, 1)
    return new_page

def merge_pdfs(paths, output, page_width, page_height):
    pdf_writer = PdfWriter()

    for path in paths:
        pdf_reader = PdfReader(path)
        for page in pdf_reader.pages:
            resized_page = resize_page(page, page_width, page_height)
            pdf_writer.add_page(resized_page)

    with open(output, 'wb') as out_pdf:
        pdf_writer.write(out_pdf)

def split_and_merge_decks(deck_a_path, deck_b_path, split_at, output_path, page_width, page_height):
    deck_a_reader = PdfReader(deck_a_path)
    part_one_writer = PdfWriter()
    part_two_writer = PdfWriter()

    # Split and resize deck A
    for page in range(split_at):
        resized_page = resize_page(deck_a_reader.pages[page], page_width, page_height)
        part_one_writer.add_page(resized_page)

    for page in range(split_at, len(deck_a_reader.pages)):
        resized_page = resize_page(deck_a_reader.pages[page], page_width, page_height)
        part_two_writer.add_page(resized_page)

    # Save the split parts temporarily
    part_one_path = 'part_one.pdf'
    part_two_path = 'part_two.pdf'
    with open(part_one_path, 'wb') as f:
        part_one_writer.write(f)
    with open(part_two_path, 'wb') as f:
        part_two_writer.write(f)

    # Merge the parts with deck B
    merge_pdfs([part_one_path, deck_b_path, part_two_path], output_path, page_width, page_height)

    # Optionally, delete temporary files
    # os.remove(part_one_path)
    # os.remove(part_two_path)

deck1 = 'LANCEr Site Visit.pdf'
deck2 = '../slides-lancer.pdf'
split_at = 25
output = 'lancer-site-visit.pdf'

deck1_reader = PdfReader(deck1)
deck2_reader = PdfReader(deck2)
writer = PdfWriter()

all_pages = \
  list(deck1_reader.pages[0:split_at]) + \
  list(deck2_reader.pages) + \
  list(deck1_reader.pages[split_at:])

# get page size from first page
page_width = deck1_reader.pages[0].mediabox.width
page_height = deck1_reader.pages[0].mediabox.height

for page in all_pages:
  # get page size
  old_page_width = page.mediabox.width
  scale_x = float(page_width) / float(old_page_width)
  old_page_height = page.mediabox.height
  scale_y = float(page_height) / float(old_page_height)
  page.scale(scale_x, scale_y)
  # add page to writer
  writer.add_page(page)

with open(output, 'wb') as out_pdf:
  writer.write(out_pdf)