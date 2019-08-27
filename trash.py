registers = ['c','d','e','h','l']

codeline = 'reg_%c[%i] = GTK_WIDGET(gtk_builder_get_object(builder, "reg_%c_%i"));'

for r in registers:
    for i in range(8):
        print(codeline%(r,i,r,i))