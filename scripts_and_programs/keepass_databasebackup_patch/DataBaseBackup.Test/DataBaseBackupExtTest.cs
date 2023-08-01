// --------------------------------------------------------------------------------------------------------------------
// <copyright file="DataBaseBackupExtTest.cs" company="">
//   
// </copyright>
// <summary>
//   Summary description for UnitTest1
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace DataBaseBackup.Test
{
    using KeePass.Plugins;

    using KeePassLib;

    using Microsoft.VisualStudio.TestTools.UnitTesting;

    using Moq;

    /// <summary>
    ///     Summary description for UnitTest1
    /// </summary>
    [TestClass]
    public class DataBaseBackupExtTest
    {
        #region Public Methods and Operators

        /// <summary>
        /// The local file with exentionkdbx.
        /// </summary>
        [TestMethod]
        public void LocalFileWithExentionkdbx()
        {
            string fileName = @"c:\test.kdbx";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetSourceFileName(mockPluginHost.Object) == "test");
        }

        /// <summary>
        /// The local file with exentiontxt.
        /// </summary>
        [TestMethod]
        public void LocalFileWithExentiontxt()
        {
            string fileName = @"c:\test.txt";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetSourceFileName(mockPluginHost.Object) == "test");
        }

        /// <summary>
        /// The log_ local file with exentionk.
        /// </summary>
        [TestMethod]
        public void Log_LocalFileWithExentionk()
        {
            string fileName = @"c:\test-test.kdbx";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetLogFileName(mockPluginHost.Object) == "test-test.kdbx_log");
        }

        /// <summary>
        /// The log_ local file with exentiontxt.
        /// </summary>
        [TestMethod]
        public void Log_LocalFileWithExentiontxt()
        {
            string fileName = @"c:\test.txt";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetLogFileName(mockPluginHost.Object) == "test.txt_log");
        }

        /// <summary>
        /// The remote file preserve dash.
        /// </summary>
        [TestMethod]
        public void RemoteFilePreserveDash()
        {
            string fileName = "ftp://ftp.test.com/test-test.kdbx";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetSourceFileName(mockPluginHost.Object) == "test-test");
        }

        /// <summary>
        /// The remote file with exention txt.
        /// </summary>
        [TestMethod]
        public void RemoteFileWithExentionTxt()
        {
            string fileName = "ftp://ftp.test.com/test.txt";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetSourceFileName(mockPluginHost.Object) == "test");
        }

        /// <summary>
        /// The remote file with exentionkdbx.
        /// </summary>
        [TestMethod]
        public void RemoteFileWithExentionkdbx()
        {
            string fileName = "ftp://ftp.test.com/test.kdbx";

            var mockPluginHost = new Mock<IPluginHost>();
            var pwData = new PwDatabase();
            pwData.IOConnectionInfo.Path = fileName;

            mockPluginHost.Setup(x => x.Database).Returns(pwData);

            Assert.IsTrue(Helper.GetSourceFileName(mockPluginHost.Object) == "test");
        }

        #endregion
    }
}