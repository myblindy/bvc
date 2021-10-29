namespace bvc.Support;

class StringStream : MemoryStream
{
    private readonly string str;
    private int position;

    public override bool CanRead => true;
    public override bool CanSeek => true;
    public override bool CanWrite => false;

    public override long Length
    {
        get
        {
            if (position < 0)
                throw new ObjectDisposedException(nameof(StringStream));
            return str.Length << 1;
        }
    }

    public override long Position
    {
        get
        {
            if (position < 0)
                throw new ObjectDisposedException(nameof(StringStream));
            return position;
        }
        set
        {
            if (position < 0)
                throw new ObjectDisposedException(nameof(StringStream));
            if (value < 0L || value > Length)
                throw new ArgumentOutOfRangeException(nameof(Position));
            position = (int)value;
        }
    }

    public override int Capacity
    {
        get
        {
            if (position < 0)
                throw new ObjectDisposedException(nameof(StringStream));
            return str.Length << 1;
        }
        set => base.Capacity = value; // to throw the appropriate exception
    }

    private byte this[int i] => (i & 1) == 0 ? (byte)(str[i >> 1] & 0xFF) : (byte)(str[i >> 1] >> 8);

    public StringStream(string s) : base(Array.Empty<byte>(), false)
    {
        str = s ?? throw new ArgumentNullException(nameof(s));
    }

    public override long Seek(long offset, SeekOrigin origin)
    {
        if (position < 0)
            throw new ObjectDisposedException(nameof(StringStream));

        switch (origin)
        {
            case SeekOrigin.Begin:
                Position = offset;
                break;
            case SeekOrigin.Current:
                Position += offset;
                break;
            case SeekOrigin.End:
                Position = Length - offset;
                break;
        }

        return position;
    }

    public override int Read(byte[] buffer, int offset, int count)
    {
        if (position < 0)
            throw new ObjectDisposedException(nameof(StringStream));
        if (offset < 0)
            throw new ArgumentOutOfRangeException(nameof(offset));
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));
        if (buffer.Length - offset < count)
            throw new ArgumentException();

        int len = Math.Min(count, (str.Length << 1) - position);
        for (int i = 0; i < len; i++)
        {
            buffer[offset] = this[position];
            offset += 1;
            position += 1;
        }

        return len;
    }

    public override int ReadByte()
    {
        if (position < 0)
            throw new ObjectDisposedException(nameof(StringStream));
        return position == str.Length << 1 ? -1 : this[position++];
    }

    public override byte[] ToArray()
    {
        var result = new byte[str.Length << 1];
        for (int i = 0; i < result.Length; i++)
            result[i] = this[i];
        return result;
    }

    public override void WriteTo(Stream stream)
    {
        if (position < 0)
            throw new ObjectDisposedException(nameof(StringStream));
        using var ss = new StringStream(str);
        ss.CopyTo(stream);
    }

    public override void SetLength(long value) => throw new NotSupportedException();
    public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();
    public override void WriteByte(byte value) => throw new NotSupportedException();

    public override string ToString() => str;

    protected override void Dispose(bool disposing)
    {
        if (position < 0)
            return;
        position = -1;
        base.Dispose(disposing);
    }
}
