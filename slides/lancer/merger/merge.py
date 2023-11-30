from PyPDF2 import PdfReader, PdfWriter

deck1 = 'LANCER Site Visit.pdf'
deck2 = '../slides-lancer.pdf'
split_at = 25
output = 'lancer-site-visit.pdf'

deck1_reader = PdfReader(deck1)
deck2_reader = PdfReader(deck2)
writer = PdfWriter()

all_pages = \
  list(deck1_reader.pages[0:split_at-1]) + \
  list(deck2_reader.pages) + \
  list(deck1_reader.pages[split_at:])

# get page size from first page to make sure all pages are the same size
page_width = deck1_reader.pages[0].mediabox.width
page_height = deck1_reader.pages[0].mediabox.height

for page in all_pages:
  old_page_width = page.mediabox.width
  scale_x = float(page_width) / float(old_page_width)
  old_page_height = page.mediabox.height
  scale_y = float(page_height) / float(old_page_height)
  page.scale(scale_x, scale_y)
  writer.add_page(page)

with open(output, 'wb') as out_pdf:
  writer.write(out_pdf)