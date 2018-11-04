import libtorrent as lt
import time

ses = lt.session()
params = { 'save_path': '/tmp/python_download'}
link = "magnet:?xt=urn:btih:7F15F37883029A8AE544FF0BD246911A5D6BD04E&tr=http%3A%2F%2Fbt3.t-ru.org%2Fann%3Fmagnet&dn=%D0%96%D0%B5%D0%BB%D0%B5%D0%B7%D0%BD%D1%8B%D0%B9%20%D1%87%D0%B5%D0%BB%D0%BE%D0%B2%D0%B5%D0%BA%20%2F%20Iron%20Man%20(%D0%94%D0%B6%D0%BE%D0%BD%20%D0%A4%D0%B0%D0%B2%D1%80%D0%BE%20%2F%20Jon%20Favreau)%20%5B2008%2C%20%D0%A1%D0%A8%D0%90%2C%20%D0%A4%D0%B0%D0%BD%D1%82%D0%B0%D1%81%D1%82%D0%B8%D0%BA%D0%B0%2C%20%D0%B1%D0%BE%D0%B5%D0%B2%D0%B8%D0%BA%2C%20%D0%BF%D1%80%D0%B8%D0%BA%D0%BB%D1%8E%D1%87%D0%B5%D0%BD%D0%B8%D1%8F%2C%20BDRip-AVC%5D%20Dub%20%2B%20Original%20Eng%20%2B%20Sub%20(Rus%2C%20Eng)";
handle = lt.add_magnet_uri(ses, link, params)

print 'downloading metadata...'
while (not handle.has_metadata()): time.sleep(1)
print 'got metadata, starting torrent download...'
while (handle.status().state != lt.torrent_status.seeding):
    print '%d %% done' % (handle.status().progress*100)
    time.sleep(3)